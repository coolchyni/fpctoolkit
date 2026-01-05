import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import { z } from "zod";
import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as http from 'http';
import * as crypto from 'crypto';
import { CompileOption } from '../languageServer/options';
import { FpcProjectProvider } from '../providers/project';
import { taskProvider } from '../providers/task';
import { ProjectType } from '../providers/projectType';
import { FpcItem } from '../providers/fpcItem';
import { FpcTask } from "../providers/fpcTaskProject";
import { ProjectTemplateManager, ProjectTemplate } from '../providers/projectTemplate';

export class PascalMcpServer {
    private server: McpServer;
    private transport: StreamableHTTPServerTransport | null = null;
    private httpServer: http.Server | null = null;
    private projectProvider: FpcProjectProvider | null = null;
    private templateManager: ProjectTemplateManager;
    private workspaceRoot: string;
    private port: number = 0; // 0 means auto-assign
    private host: string = 'localhost';

    constructor(workspaceRoot: string, projectProvider?: FpcProjectProvider, port?: number, host?: string) {
        this.workspaceRoot = workspaceRoot;
        this.projectProvider = projectProvider || null;
        this.templateManager = new ProjectTemplateManager(workspaceRoot);
        this.port = port || 0; // Use 0 to let system auto-assign available port
        this.host = host || 'localhost';
        
        // Get package.json version
        const packageJsonPath = path.join(path.dirname(path.dirname(__dirname)), 'package.json');
        let version = '1.0.0';
        try {
            if (fs.existsSync(packageJsonPath)) {
                const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
                version = packageJson.version || '1.0.0';
            }
        } catch (e) {
            console.error('Failed to read package.json version:', e);
        }
        
        // Create MCP server instance
        this.server = new McpServer({
            name: "pascal-mcp-server",
            version: version
        });

        this.registerTools();
        
    }

    private registerTools() {
        //Register the get_compile_command tool with parameters
        this.server.tool(
            "getCompileCommand",
            "Get the compile command for current project. Use this tool to get compile command if user ask to build the project.",
            {
            },
            async ({  }) => {
                try {
                    const compileCommand = await this.getCompileCommand(undefined);
                    return {
                        content: [
                            {
                                type: "text",
                                text: compileCommand
                            }
                        ]
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: "text",
                                text: `Error getting compile command: ${error instanceof Error ? error.message : 'Unknown error'}`
                            }
                        ]
                    };
                }
            }
        );

        // Register the compile_project tool
        this.server.tool(
            "compileProject",
            "Compile the current Pascal project",
            {},
            async () => {
                try {
                    const result = await this.compileProject();
                    return {
                        content: [
                            {
                                type: "text",
                                text: result
                            }
                        ]
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: "text",
                                text: `Error compiling project: ${error instanceof Error ? error.message : 'Unknown error'}`
                            }
                        ]
                    };
                }
            }
        );

        // Register the get_project_templates tool
        this.server.tool(
            "getProjectTemplates",
            "Get list of available Pascal project templates",
            {},
            async () => {
                try {
                    const templates = await this.getProjectTemplates();
                    return {
                        content: [
                            {
                                type: "text",
                                text: JSON.stringify(templates, null, 2)
                            }
                        ]
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: "text",
                                text: `Error getting project templates: ${error instanceof Error ? error.message : 'Unknown error'}`
                            }
                        ]
                    };
                }
            }
        );

        // Register the create_project tool
        this.server.tool(
            "createProject",
            "Create a new Pascal project from a template. Use getProjectTemplates tool first to get available template names.",
            {
                templateName: z.string().describe("Name of the template to use (get available names from getProjectTemplates tool)"),
                projectName: z.string().describe("Name for the new project (should be in English, no spaces or special characters)")
            },
            async (params: { templateName: string; projectName: string }) => {
                try {
                    const result = await this.createProjectFromTemplate(
                        params.templateName,
                        params.projectName
                    );
                    return {
                        content: [
                            {
                                type: "text" as const,
                                text: result
                            }
                        ]
                    };
                } catch (error) {
                    return {
                        content: [
                            {
                                type: "text" as const,
                                text: `Error creating project: ${error instanceof Error ? error.message : 'Unknown error'}`
                            }
                        ]
                    };
                }
            }
        );

    }

    /**
     * Compile the current Pascal project using VS Code tasks
     */
    private async compileProject(): Promise<string> {
        try {
            // Check if project provider is available
            if (!this.projectProvider) {
                throw new Error("No project provider available");
            }

            const projectItems = await this.projectProvider.getChildren() as FpcItem[];
            if (!projectItems || projectItems.length === 0) {
                throw new Error("No projects found in workspace");
            }

            // Get the default project
            const defaultProject = this.projectProvider.defaultFpcItem || projectItems[0];
            
            // Get the project task
            const projectTask = defaultProject?.projectTask;
            if (!projectTask) {
                throw new Error("No build task found for the project");
            }

            // Get the VS Code task from the project task
            const vscodeTask = await projectTask.getTask();

            // Execute the VS Code task and capture detailed output
            return new Promise((resolve, reject) => {
                let taskTerminal: vscode.Terminal | undefined;
                let compileOutput: string[] = [];
                let hasErrors = false;
                let hasWarnings = false;
                let errorCount = 0;
                let warningCount = 0;

                vscode.tasks.executeTask(vscodeTask).then((taskExecution) => {
                    // Try to find the terminal associated with this task
                    const findTerminal = () => {
                        const terminals = vscode.window.terminals;
                        for (const terminal of terminals) {
                            if (terminal.name.includes(vscodeTask.name) || 
                                terminal.name.includes('Task') ||
                                terminal.name.includes('Build') ||
                                terminal.name.includes('fpc')) {
                                return terminal;
                            }
                        }
                        return terminals.length > 0 ? terminals[terminals.length - 1] : undefined;
                    };

                    // Wait for terminal to be created
                    setTimeout(() => {
                        taskTerminal = findTerminal();
                    }, 500);

                    // Listen for task completion
                    const endDisposable = vscode.tasks.onDidEndTask((e) => {
                        if (e.execution === taskExecution) {
                            endDisposable.dispose();
                            startDisposable.dispose();
                            clearTimeout(timeoutHandle);
                            
                            // Get task information
                            const taskName = vscodeTask.name || 'Build';
                            let result = `${taskName} task completed\n`;
                            
                            // Add execution details
                            if (e.execution.task.execution) {
                                if (e.execution.task.execution instanceof vscode.ProcessExecution) {
                                    const processExecution = e.execution.task.execution;
                                    result += `Executed: ${processExecution.process}`;
                                    if (processExecution.args && processExecution.args.length > 0) {
                                        result += ` ${processExecution.args.join(' ')}`;
                                    }
                                } else if (e.execution.task.execution instanceof vscode.ShellExecution) {
                                    const shellExecution = e.execution.task.execution;
                                    if (typeof shellExecution.command === 'string') {
                                        result += `Executed: ${shellExecution.command}`;
                                        if (shellExecution.args) {
                                            if (Array.isArray(shellExecution.args)) {
                                                result += ` ${shellExecution.args.join(' ')}`;
                                            } else {
                                                result += ` ${shellExecution.args}`;
                                            }
                                        }
                                    }
                                }
                            }
                            
                            if (taskTerminal) {
                                result += `\nTerminal: ${taskTerminal.name}`;
                            }
                            
                            // Check diagnostics for detailed compilation status
                            setTimeout(() => {
                                const diagnostics = vscode.languages.getDiagnostics();
                                errorCount = 0;
                                warningCount = 0;
                                
                                // Count errors and warnings from diagnostics
                                for (const [uri, uriDiagnostics] of diagnostics) {
                                    if (uri.scheme === 'file') {
                                        for (const diagnostic of uriDiagnostics) {
                                            if (diagnostic.severity === vscode.DiagnosticSeverity.Error) {
                                                errorCount++;
                                                hasErrors = true;
                                            } else if (diagnostic.severity === vscode.DiagnosticSeverity.Warning) {
                                                warningCount++;
                                                hasWarnings = true;
                                            }
                                        }
                                    }
                                }
                                
                                // Determine compilation status
                                if (errorCount > 0) {
                                    result += `\n\n✗ Compilation FAILED`;
                                    result += `\n  ${errorCount} error(s), ${warningCount} warning(s)`;
                                } else if (warningCount > 0) {
                                    result += `\n\n✓ Compilation SUCCEEDED with warnings`;
                                    result += `\n  ${warningCount} warning(s)`;
                                } else {
                                    result += `\n\n✓ Compilation SUCCEEDED`;
                                    result += `\n  No errors or warnings`;
                                }
                                
                                // Add detailed diagnostic information if available
                                if (errorCount > 0 || warningCount > 0) {
                                    result += `\n\nCheck the Problems panel (View → Problems) for detailed error/warning information.`;
                                }
                                
                                if (taskTerminal) {
                                    result += `\nCheck terminal '${taskTerminal.name}' for complete build output.`;
                                }
                                
                                resolve(result);
                            }, 1500); // Wait for diagnostics to be updated
                        }
                    });

                    // Listen for task start
                    const startDisposable = vscode.tasks.onDidStartTask((e) => {
                        if (e.execution === taskExecution) {
                            console.log(`Compilation task started: ${vscodeTask.name}`);
                        }
                    });

                    // Set a timeout to avoid hanging indefinitely
                    const timeoutHandle = setTimeout(() => {
                        endDisposable.dispose();
                        startDisposable.dispose();
                        
                        let result = `Compilation task timed out after 30 seconds`;
                        if (taskTerminal) {
                            result += `\nTask may still be running in terminal '${taskTerminal.name}'`;
                        }
                        result += `\nCheck VS Code terminal and Problems panel for current status.`;
                        resolve(result);
                    }, 30000); // 30 second timeout

                }, (error) => {
                    reject(new Error(`Failed to execute build task: ${error.message}`));
                });
            });

        } catch (error) {
            throw new Error(`Error compiling project: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }

    private extractUsesList(fileContent: string): string[] {
        const usesList: string[] = [];
        // Match 'uses' sections in Pascal code
        const usesRegex = /\buses\b(.*?);/gi;
        let match;
        
        while ((match = usesRegex.exec(fileContent)) !== null) {
            if (match[1]) {
                // Split the uses list by commas and clean up whitespace
                const units = match[1].split(',').map(unit => unit.trim());
                usesList.push(...units);
            }
        }
        
        return usesList;
    }

    private async getCompileCommand(filePath?: string): Promise<string> {
        try {
            let targetFilePath = '';
            let relativePath = '';
            
            // Process filePath if provided
            if (filePath) {
                // Resolve absolute path if needed
                let absolutePath = filePath;
                if (!path.isAbsolute(filePath)) {
                    absolutePath = path.resolve(this.workspaceRoot, filePath);
                }

                // Get file extension to determine file type
                const ext = path.extname(absolutePath).toLowerCase();
                const validExtensions = ['.pas', '.pp', '.lpr', '.dpr'];
                
                if (!validExtensions.includes(ext)) {
                    throw new Error(`Unsupported file extension: ${ext}. Supported extensions: ${validExtensions.join(', ')}`);
                }

                // Get relative path from workspace root
                relativePath = path.relative(this.workspaceRoot, absolutePath);
                targetFilePath = relativePath;
            }

            // Try to find compile options from project provider
            let compileOption: CompileOption | null = null;

            if (this.projectProvider) {
                try {
                    compileOption = await this.projectProvider.GetDefaultTaskOption();
                    
                    // If filePath is provided and compileOption exists, update the file path
                    if (filePath && compileOption) {
                        // Save the original file property
                        const originalFile = compileOption.file;
                        
                        // Update with our target file
                        compileOption.file = targetFilePath;
                        
                        // Get compile command with our file
                        const result = this.buildCompileCommand(compileOption);
                        
                        // Restore original file property
                        compileOption.file = originalFile;
                        
                        return result;
                    }
                } catch (error) {
                    console.error('Error getting default task option:', error);
                }
            }

            // If no compile option found, try different approaches
            if (!compileOption) {
                // If we have a filePath, try to create a basic compilation command
                if (filePath) {
                    const defaultCompileOption = new CompileOption({
                        type: 'fpc',
                        file: relativePath,
                        buildOption: {
                            unitOutputDir: './out',
                            customOptions: ['-dDEBUG']
                        }
                    }, this.workspaceRoot);
                    
                    return this.buildCompileCommand(defaultCompileOption);
                }
                throw new Error("No compile option found. Please provide a valid filePath or configure a project.");
            }

            return this.buildCompileCommand(compileOption);
        } catch (error) {
            console.error('Error in getCompileCommand:', error);
            throw error;
        }
    }
    
    /**
     * Build the compile command from a CompileOption object
     */
    private buildCompileCommand(compileOption: CompileOption): string {
        // Get FPC compiler path
        const fpcPath = process.env['PP'] || 'fpc';

        // Build the compile command
        const optionString = compileOption.toOptionString();
        const compileCommand = `${fpcPath} ${optionString} ${compileOption.file}`;

        return compileCommand.trim();
    }
    
    /**
     * Get list of available project templates
     */
    private async getProjectTemplates(): Promise<any> {
        try {
            const templates = await this.templateManager.getAvailableTemplates();
            
            // Convert templates to a simplified format with only name and description
            const templateList = templates.map(template => ({
                name: template.name,
                description: template.description
            }));

            return {
                count: templateList.length,
                templates: templateList,
                hint: "Use createProject tool to create a new project from any of these templates"
            };
        } catch (error) {
            console.error('Error getting project templates:', error);
            throw error;
        }
    }

    /**
     * Create a new project from template
     */
    private async createProjectFromTemplate(templateName: string, projectName: string, projectPath?: string): Promise<string> {
        try {
            if (!templateName) {
                throw new Error("Template name is required");
            }
            
            if (!projectName) {
                throw new Error("Project name is required");
            }

            // Get available templates
            const templates = await this.templateManager.getAvailableTemplates();
            
            // Find the specified template
            const template = templates.find(t => t.name === templateName);
            if (!template) {
                const availableNames = templates.map(t => t.name).join(', ');
                throw new Error(`Template "${templateName}" not found. Available templates: ${availableNames}`);
            }

            // Determine target directory
            let targetDir = this.workspaceRoot;
            if (projectPath) {
                if (path.isAbsolute(projectPath)) {
                    targetDir = projectPath;
                } else {
                    targetDir = path.join(this.workspaceRoot, projectPath);
                }
            }

            // Create the project
            await this.templateManager.createProjectFromTemplate(template, projectName, targetDir);

            const createdFiles = template.files?.map(f => f.path) || [];
            
            let result = `✅ Project "${projectName}" created successfully from template "${templateName}"`;
            result += `\n📁 Location: ${targetDir}`;
            result += `\n📄 Files created: ${createdFiles.length}`;
            
            if (createdFiles.length > 0) {
                result += `\n   - ${createdFiles.join('\n   - ')}`;
            }

            if (template.tasks && template.tasks.length > 0) {
                result += `\n⚙️  Tasks configured: ${template.tasks.length}`;
            }

            // Refresh project provider if available
            if (this.projectProvider) {
                try {
                    await this.projectProvider.refresh();
                } catch (error) {
                    console.error('Error refreshing project provider:', error);
                }
            }

            return result;
        } catch (error) {
            console.error('Error creating project from template:', error);
            throw error;
        }
    }
    
    /**
     * Get information about the current project
     */
    private async getProjectInfo(): Promise<any> {
        try {
            if (!this.projectProvider) {
                return {
                    error: "No project provider available",
                    type: "unknown",
                    hasProject: false
                };
            }
            
            // Get projects by examining the tree items
          
            const projectItems = await this.projectProvider.getChildren() as FpcItem[];
            if (!projectItems || projectItems.length === 0) {
                return {
                    error: "No projects found in workspace",
                    type: "unknown",
                    hasProject: false
                };
            }
            
            // Get info about default project
            const defaultProject = this.projectProvider.defaultFpcItem || projectItems[0];
            const projectType = defaultProject.projectType;
            
            let projectInfo: any = {
                hasProject: true,
                type: projectType,
                name: defaultProject.label || "Unknown",
                filePath: defaultProject.file || "",
            };
            
            // Get build modes if available (for Lazarus projects)
            if (defaultProject.project && defaultProject.project.tasks) {
                projectInfo.buildModes = defaultProject.project.tasks.map((task: any) => ({
                    name: task.label || "",
                    isDefault: task.isDefault || false
                }));
            }
            
            // Add compile options
            try {
                const compileOption = await this.projectProvider.GetDefaultTaskOption();
                if (compileOption) {
                    projectInfo.compileOptions = {
                        unitOutputDir: compileOption.buildOption?.unitOutputDir || "",
                        outputFile: compileOption.buildOption?.outputFile || "",
                        syntaxMode: compileOption.buildOption?.syntaxMode || "ObjFPC",
                        targetOS: compileOption.buildOption?.targetOS || "",
                        targetCPU: compileOption.buildOption?.targetCPU || "",
                    };
                }
            } catch (error) {
                projectInfo.compileOptionsError = "Failed to get compile options";
            }
            
            return projectInfo;
        } catch (error) {
            console.error('Error in getProjectInfo:', error);
            throw error;
        }
    }

    async start(): Promise<void> {
        try {
            // Create StreamableHTTPServerTransport with session support
            this.transport = new StreamableHTTPServerTransport({
                sessionIdGenerator: () => crypto.randomBytes(16).toString('hex'),
                onsessioninitialized: (sessionId: string) => {
                    console.log(`MCP session initialized: ${sessionId}`);
                },
                onsessionclosed: (sessionId: string) => {
                    console.log(`MCP session closed: ${sessionId}`);
                },
                enableJsonResponse: false, // Use SSE streaming
                allowedHosts: ['localhost', '127.0.0.1'],
                allowedOrigins: ['*'],
                enableDnsRebindingProtection: false
            });

            // Set up event handlers
            this.transport.onclose = () => {
                console.log('MCP transport connection closed');
            };
            
            this.transport.onerror = (error: Error) => {
                console.error('MCP transport error:', error);
            };

            // Connect the server to the transport
            await this.server.connect(this.transport);

            // Create HTTP server
            this.httpServer = http.createServer((req, res) => {
                // Enable CORS
                res.setHeader('Access-Control-Allow-Origin', '*');
                res.setHeader('Access-Control-Allow-Methods', 'GET, POST, DELETE, OPTIONS');
                res.setHeader('Access-Control-Allow-Headers', 'Content-Type, X-Session-Id');

                if (req.method === 'OPTIONS') {
                    res.writeHead(200);
                    res.end();
                    return;
                }

                // Handle all MCP requests through the transport
                if (req.method === 'POST') {
                    let body = '';
                    req.on('data', chunk => {
                        body += chunk.toString();
                    });
                    
                    req.on('end', () => {
                        try {
                            const parsedBody = JSON.parse(body);
                            this.transport!.handleRequest(req, res, parsedBody);
                        } catch (error) {
                            this.transport!.handleRequest(req, res);
                        }
                    });
                } else {
                    // Handle GET and DELETE requests
                    this.transport!.handleRequest(req, res);
                }
            });
            
            // Listen on available port (0 means auto-assign)
            await new Promise<void>((resolve, reject) => {
                this.httpServer!.listen(this.port, this.host, () => {
                    const address = this.httpServer!.address();
                    if (address && typeof address === 'object') {
                        this.port = address.port; // Get the actual assigned port
                        console.error(`Pascal MCP Server running on http://${this.host}:${this.port}`);
                        console.error(`MCP endpoint: http://${this.host}:${this.port}/mcp`);
                        resolve();
                    } else {
                        reject(new Error('Failed to get server address'));
                    }
                });
                
                this.httpServer!.on('error', (error: any) => {
                    if (error.code === 'EADDRINUSE') {
                        console.error(`Port ${this.port} is already in use, trying another port...`);
                        // If specified port is in use, try auto-assignment
                        this.port = 0;
                        this.httpServer!.listen(0, this.host);
                    } else {
                        reject(error);
                    }
                });
            });
            
        } catch (error) {
            console.error("Failed to start Pascal MCP Server:", error);
            throw error;
        }
    }

    async stop(): Promise<void> {
        try {
            if (this.transport) {
                await this.transport.close();
                this.transport = null;
            }
            
            if (this.httpServer) {
                await new Promise<void>((resolve) => {
                    this.httpServer!.close(() => {
                        this.httpServer = null;
                        resolve();
                    });
                });
            }
        } catch (error) {
            console.error("Error stopping Pascal MCP Server:", error);
        }
    }

    /**
     * Get the actual port the server is running on
     */
    getPort(): number {
        return this.port;
    }

    /**
     * Get the server URL
     */
    getServerUrl(): string {
        return `http://${this.host}:${this.port}`;
    }

    /**
     * Get the MCP endpoint URL
     */
    getMcpUrl(): string {
        return `http://${this.host}:${this.port}/mcp`;
    }

    /**
     * Check if the server is running
     */
    isRunning(): boolean {
        return this.httpServer !== null && this.transport !== null;
    }

    /**
     * Get server status information
     */
    getServerStatus(): { running: boolean; url?: string; mcpUrl?: string; port?: number } {
        if (this.isRunning()) {
            return {
                running: true,
                url: this.getServerUrl(),
                mcpUrl: this.getMcpUrl(),
                port: this.getPort()
            };
        }
        return { running: false };
    }

    setProjectProvider(projectProvider: FpcProjectProvider) {
        this.projectProvider = projectProvider;
    }
}
