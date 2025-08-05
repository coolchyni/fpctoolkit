import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as fs2 from 'fs-extra';

export interface ProjectTemplate {
    name: string;
    description: string;
    files: TemplateFile[];
    tasks?: any[];
}

export interface TemplateFile {
    path: string;
    content: string;
    cursorPosition?: { line: number; column: number };
}

export class ProjectTemplateManager {
    private static readonly DEFAULT_TEMPLATE_DIR = 'templates';
    private workspaceRoot: string;

    constructor(workspaceRoot: string) {
        this.workspaceRoot = workspaceRoot;
    }

    /**
     * Get template directory paths
     */
    private getTemplateDirectories(): string[] {
        const config = vscode.workspace.getConfiguration('fpctoolkit');
        const customTemplateDirs = config.get<string[]>('project.templateDirectories', []);
        
        const directories: string[] = [];
        
        // Add user global template directory
        const userTemplateDir = this.getUserTemplateDirectory();
        if (fs.existsSync(userTemplateDir)) {
            directories.push(userTemplateDir);
        }
        
        // Add custom template directories
        for (const dir of customTemplateDirs) {
            if (path.isAbsolute(dir)) {
                directories.push(dir);
            } else {
                directories.push(path.join(this.workspaceRoot, dir));
            }
        }
        
        // Add workspace default template directory (backward compatibility)
        const defaultDir = path.join(this.workspaceRoot, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        if (fs.existsSync(defaultDir) && !directories.includes(defaultDir)) {
            directories.push(defaultDir);
        }
        
        return directories;
    }

    /**
     * Get user global template directory
     */
    private getUserTemplateDirectory(): string {
        const os = require('os');
        return path.join(os.homedir(), '.fpctoolkit', 'templates');
    }

    /**
     * Get all available project templates
     */
    async getAvailableTemplates(): Promise<ProjectTemplate[]> {
        const templates: ProjectTemplate[] = [];
        
        // First scan external template directories
        const templateDirs = this.getTemplateDirectories();
        
        for (const templateDir of templateDirs) {
            if (fs.existsSync(templateDir)) {
                const templateFolders = fs.readdirSync(templateDir, { withFileTypes: true })
                    .filter(dirent => dirent.isDirectory())
                    .map(dirent => dirent.name);
                
                for (const templateFolder of templateFolders) {
                    const templatePath = path.join(templateDir, templateFolder);
                    const template = await this.loadTemplate(templatePath);
                    if (template) {
                        templates.push(template);
                    }
                }
            }
        }
        
        // If no external templates found, try to load built-in templates from extension
        if (templates.length === 0) {
            const builtInTemplates = await this.loadBuiltInTemplates();
            templates.push(...builtInTemplates);
        }
        
        // If no templates found at all, use fallback default template
        if (templates.length === 0) {
            templates.push(this.getDefaultFpcTemplate());
        }
        
        return templates;
    }

    /**
     * Load built-in templates from extension
     */
    private async loadBuiltInTemplates(): Promise<ProjectTemplate[]> {
        const templates: ProjectTemplate[] = [];
        
        const extensionPath = vscode.extensions.getExtension('coolchyni.fpctoolkit')?.extensionPath;
        if (!extensionPath) {
            return templates;
        }
        
        const extensionTemplateDir = path.join(extensionPath, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        if (!fs.existsSync(extensionTemplateDir)) {
            return templates;
        }
        
        try {
            const templateFolders = fs.readdirSync(extensionTemplateDir, { withFileTypes: true })
                .filter(dirent => dirent.isDirectory())
                .map(dirent => dirent.name);
            
            for (const templateFolder of templateFolders) {
                const templatePath = path.join(extensionTemplateDir, templateFolder);
                const template = await this.loadTemplate(templatePath);
                if (template) {
                    templates.push(template);
                }
            }
        } catch (error) {
            console.error(`Failed to load built-in templates from ${extensionTemplateDir}:`, error);
        }
        
        return templates;
    }

    /**
     * Get default FPC project template (ultimate fallback)
     */
    private getDefaultFpcTemplate(): ProjectTemplate {
        return {
            name: 'New FPC Project',
            description: 'Create a standard FPC project with main.lpr',
            files: [
                {
                    path: 'main.lpr',
                    content: `program main;
{$mode objfpc}{$H+}
uses
  classes,sysutils;
begin 
   
end.`,
                    cursorPosition: { line: 5, column: 4 }
                }
            ],
            tasks: [
                {
                    "label": "debug",
                    "file": "main.lpr",
                    "type": "fpc",
                    "presentation": {
                        "showReuseMessage": false,
                        "clear": true,
                        "revealProblems": "onProblem"
                    },
                    "buildOption": {
                        "unitOutputDir": "./out",
                        "customOptions": [
                            "-dDEBUG"
                        ]
                    }
                }
            ]
        };
    }

    /**
     * Load template from directory
     */
    private async loadTemplate(templatePath: string): Promise<ProjectTemplate | null> {
        try {
            const configPath = path.join(templatePath, 'template.json');
            if (!fs.existsSync(configPath)) {
                return null;
            }

            const configContent = fs.readFileSync(configPath, 'utf8');
            const config = JSON.parse(configContent);

            const template: ProjectTemplate = {
                name: config.name || path.basename(templatePath),
                description: config.description || '',
                files: [],
                tasks: config.tasks
            };

            // Load template files
            if (config.files && Array.isArray(config.files)) {
                for (const fileConfig of config.files) {
                    const filePath = path.join(templatePath, fileConfig.source);
                    if (fs.existsSync(filePath)) {
                        let content = fs.readFileSync(filePath, 'utf8');
                        
                        template.files.push({
                            path: fileConfig.target || fileConfig.source,
                            content: content,
                            cursorPosition: fileConfig.cursorPosition
                        });
                    }
                }
            }

            return template;
        } catch (error) {
            console.error(`Failed to load template from ${templatePath}:`, error);
            return null;
        }
    }

    /**
     * Process template variables
     */
    private processTemplateVariables(content: string, projectName?: string): string {
        const now = new Date();
        const variables: { [key: string]: string } = {
            '{{DATE}}': now.toLocaleDateString(),
            '{{TIME}}': now.toLocaleTimeString(),
            '{{YEAR}}': now.getFullYear().toString(),
            '{{MONTH}}': (now.getMonth() + 1).toString().padStart(2, '0'),
            '{{DAY}}': now.getDate().toString().padStart(2, '0'),
            '{{USER}}': process.env.USER || process.env.USERNAME || 'User',
            '{{PROJECT_NAME}}': projectName || 'newproject'
        };

        let result = content;
        for (const [variable, value] of Object.entries(variables)) {
            result = result.replace(new RegExp(variable, 'g'), value);
        }

        return result;
    }

    /**
     * Create project from template
     */
    async createProjectFromTemplate(template: ProjectTemplate, projectName?: string, targetDir?: string): Promise<void> {
        try {
            const finalProjectName = projectName || 'newproject';
            const projectDir = targetDir || this.workspaceRoot;
            
            // Create files
            for (const file of template.files) {
                let content = file.content;
                let targetPath = file.path;
                
                // Process template variables
                content = this.processTemplateVariables(content, finalProjectName);
                targetPath = this.processTemplateVariables(targetPath, finalProjectName);
                
                const filePath = path.join(projectDir, targetPath);
                const fileDir = path.dirname(filePath);
                
                // Ensure directory exists
                if (!fs.existsSync(fileDir)) {
                    fs2.ensureDirSync(fileDir);
                }
                
                // Write file
                fs.writeFileSync(filePath, content);
            }

            // Add task configuration
            if (template.tasks && template.tasks.length > 0) {
                // Process variables in tasks
                const processedTasks = template.tasks.map(task => {
                    const processedTask = JSON.parse(JSON.stringify(task)); // Deep copy
                    if (processedTask.file) {
                        processedTask.file = this.processTemplateVariables(processedTask.file, finalProjectName);
                    }
                    return processedTask;
                });
                await this.addTasksToWorkspace(processedTasks, projectDir);
            }

            // Open main file
            const mainFile = template.files.find(f => 
                f.cursorPosition || 
                f.path.endsWith('.lpr') || 
                f.path.endsWith('.dpr') || 
                f.path.endsWith('.pas')
            );

            if (mainFile) {
                const targetPath = this.processTemplateVariables(mainFile.path, finalProjectName);
                const filePath = path.join(projectDir, targetPath);
                const doc = await vscode.workspace.openTextDocument(filePath);
                const editor = await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
                
                if (mainFile.cursorPosition) {
                    const pos = new vscode.Position(mainFile.cursorPosition.line, mainFile.cursorPosition.column);
                    editor.selection = new vscode.Selection(pos, pos);
                }
            }

            vscode.window.showInformationMessage(`Project "${finalProjectName}" created from template: ${template.name}`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to create project from template: ${error}`);
        }
    }

    /**
     * Add tasks to workspace
     */
    private async addTasksToWorkspace(tasks: any[], projectDir?: string): Promise<void> {
        const workspaceDir = projectDir || this.workspaceRoot;
        const config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(workspaceDir));
        let existingTasks = config.get<any[]>('tasks', []);
        
        for (const task of tasks) {
            // Check if task with same name already exists
            let finalLabel = task.label;
            const currentProjectName = path.basename(task.file, path.extname(task.file));
            
            // Find duplicate tasks
            const duplicateTasks = existingTasks.filter(t => t.label === task.label);
            
            if (duplicateTasks.length > 0) {
                // Check if there are tasks from different projects
                const differentProjectTask = duplicateTasks.find(t => {
                    const taskProjectName = path.basename(t.file, path.extname(t.file));
                    return taskProjectName !== currentProjectName;
                });
                
                if (differentProjectTask) {
                    // If there are tasks from different projects, add project name suffix
                    finalLabel = `${task.label}-${currentProjectName}`;
                    
                    // Check if it's still duplicate after adding project name suffix
                    if (existingTasks.some(t => t.label === finalLabel)) {
                        console.warn(`Task "${finalLabel}" already exists. Skipping task creation.`);
                        continue; // Skip this task
                    }
                } else {
                    // If all duplicate tasks are from the same project, skip this task
                    console.warn(`Task "${task.label}" already exists for this project. Skipping task creation.`);
                    continue; // Skip this task
                }
            }
            
            // Update task label
            task.label = finalLabel;
            existingTasks.push(task);
        }
        
        await config.update('tasks', existingTasks, vscode.ConfigurationTarget.WorkspaceFolder);
    }

    /**
     * Initialize default template directory structure
     */
    async initializeDefaultTemplates(toUserSpace: boolean = false): Promise<void> {
        const templateDir = toUserSpace 
            ? this.getUserTemplateDirectory()
            : path.join(this.workspaceRoot, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        
        if (!fs.existsSync(templateDir)) {
            fs2.ensureDirSync(templateDir);
            
            // Create example templates
            await this.createExampleTemplate(templateDir);
        }
    }

    /**
     * Create example templates by copying from extension
     */
    private async createExampleTemplate(templateDir: string): Promise<void> {
        // Get extension templates directory path
        const extensionPath = vscode.extensions.getExtension('coolchyni.fpctoolkit')?.extensionPath;
        if (!extensionPath) {
            console.warn('Extension path not found');
            return;
        }
        
        const extensionTemplateDir = path.join(extensionPath, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        
        if (!fs.existsSync(extensionTemplateDir)) {
            console.warn(`Extension templates directory not found: ${extensionTemplateDir}`);
            return;
        }
        
        try {
            // Copy entire templates directory to target location
            await fs2.copy(extensionTemplateDir, templateDir, {
                overwrite: false, // Don't overwrite existing files
                errorOnExist: false // Don't error if target files exist
            });
            
            console.log(`Templates copied from ${extensionTemplateDir} to ${templateDir}`);
        } catch (error) {
            console.error(`Failed to copy templates from ${extensionTemplateDir} to ${templateDir}:`, error);
        }
    }

    /**
     * Open template directory
     */
    async openTemplateDirectory(): Promise<void> {
        const templateDirs = this.getTemplateDirectories();
        
        if (templateDirs.length === 0) {
            vscode.window.showInformationMessage('No template directories found.');
            return;
        }

        if (templateDirs.length === 1) {
            // Only one directory, open it directly
            await this.openDirectoryInExplorer(templateDirs[0]);
        } else {
            // Multiple directories, let user choose
            const items = templateDirs.map(dir => ({
                label: path.basename(dir),
                description: dir,
                detail: fs.existsSync(dir) ? 'Directory exists' : 'Directory not found',
                path: dir
            }));

            const selected = await vscode.window.showQuickPick(items, {
                placeHolder: 'Select template directory to open'
            });

            if (selected) {
                await this.openDirectoryInExplorer(selected.path);
            }
        }
    }

    /**
     * Open directory in file explorer
     */
    private async openDirectoryInExplorer(dirPath: string): Promise<void> {
        try {
            if (!fs.existsSync(dirPath)) {
                const createChoice = await vscode.window.showInformationMessage(
                    `Directory does not exist: ${dirPath}. Would you like to create it?`,
                    'Create Directory', 'Cancel'
                );
                
                if (createChoice === 'Create Directory') {
                    fs2.ensureDirSync(dirPath);
                } else {
                    return;
                }
            }

            const dirUri = vscode.Uri.file(dirPath);
            await vscode.commands.executeCommand('revealFileInOS', dirUri);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to open directory: ${error}`);
        }
    }
}