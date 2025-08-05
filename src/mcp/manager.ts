import * as vscode from 'vscode';
import { PascalMcpServer } from './server';
import { FpcProjectProvider } from '../providers/project';

export class McpManager {
    private mcpServer: PascalMcpServer | null = null;
    private context: vscode.ExtensionContext;
    private workspaceRoot: string;
    private projectProvider: FpcProjectProvider | null = null;
    private autoRegisterEnabled: boolean = false;
    private mcpServerDisposable: vscode.Disposable | null = null;
    private didChangeEmitter = new vscode.EventEmitter<void>();

    constructor(context: vscode.ExtensionContext, workspaceRoot: string) {
        this.context = context;
        this.workspaceRoot = workspaceRoot;
        
        // Listen for configuration changes
        this.context.subscriptions.push(
            vscode.workspace.onDidChangeConfiguration(this.onConfigurationChanged.bind(this))
        );
        
        // Read configuration settings
        this.updateConfigSettings();
    }

    async initialize(projectProvider: FpcProjectProvider): Promise<void> {
        this.projectProvider = projectProvider;
        
        // Check if MCP API is available before proceeding
        if (!this.isMcpApiAvailable()) {
            console.log('MCP API not available, skipping MCP initialization');
            return;
        }

        // Check if MCP server should be started based on configuration
        if (this.isMcpEnabled()) {
            await this.startMcpServer();
            // Register the MCP server definition provider first
            await this.registerServer();
        }
    }

    private isMcpEnabled(): boolean {
        const config = vscode.workspace.getConfiguration('fpctoolkit');
        return config.get<boolean>('mcp.enabled', false);
    }
    
    private updateConfigSettings(): void {
        const config = vscode.workspace.getConfiguration('fpctoolkit');
        this.autoRegisterEnabled = config.get<boolean>('mcp.autoRegister', true);
    }

    private async startMcpServer(): Promise<void> {
        if (this.mcpServer) {
            return; // Already running
        }

        try {
            // Read port settings from configuration, default to 0 (auto-assign)
            const config = vscode.workspace.getConfiguration('fpctoolkit');
            const preferredPort = config.get<number>('mcp.port', 0);
            
            this.mcpServer = new PascalMcpServer(
                this.workspaceRoot, 
                this.projectProvider || undefined, 
                preferredPort
            );
            await this.mcpServer.start();
            
            const serverStatus = this.mcpServer.getServerStatus();
            console.log(`Pascal MCP Server started on ${serverStatus.url}`);
            console.log(`MCP endpoint: ${serverStatus.mcpUrl}`);
            
            // Trigger server definitions update
            this.didChangeEmitter.fire();
            
            // Show information message with actual port and endpoints
            // vscode.window.showInformationMessage(
            //     `Pascal MCP Server started on port ${serverStatus.port}\nMCP: ${serverStatus.mcpUrl}`
            // );
            
        } catch (error) {
            console.error('Failed to start Pascal MCP Server:', error);
            vscode.window.showErrorMessage(`Failed to start Pascal MCP Server: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }

    private async stopMcpServer(): Promise<void> {
        if (!this.mcpServer) {
            return; // Not running
        }

        try {
            await this.mcpServer.stop();
            this.mcpServer = null;
            
            console.log('Pascal MCP Server stopped successfully');
            vscode.window.showInformationMessage('Pascal MCP Server stopped');
            
            // Trigger server definitions update
            this.didChangeEmitter.fire();
            
        } catch (error) {
            console.error('Failed to stop Pascal MCP Server:', error);
            vscode.window.showErrorMessage(`Failed to stop Pascal MCP Server: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }

    private async onConfigurationChanged(event: vscode.ConfigurationChangeEvent): Promise<void> {
        // Update all configuration settings
        if (event.affectsConfiguration('fpctoolkit.mcp')) {
            this.updateConfigSettings();
        }
        
        // Handle MCP server enable/disable
        if (event.affectsConfiguration('fpctoolkit.mcp.enabled')) {
            const mcpEnabled = this.isMcpEnabled();
            
            if (mcpEnabled && !this.mcpServer) {
                // MCP was enabled, start server
                await this.startMcpServer();
            } else if (!mcpEnabled && this.mcpServer) {
                // MCP was disabled, stop server
                await this.stopMcpServer();
            }
        }
        
        // Handle auto-registration changes
        if (event.affectsConfiguration('fpctoolkit.mcp.autoRegister') && this.mcpServer) {
            if (this.autoRegisterEnabled && this.isMcpApiAvailable()) {
                await this.registerMcpServer();
            }
        }
        
        // Handle port changes - restart server if port configuration changed
        if (event.affectsConfiguration('fpctoolkit.mcp.port') && this.mcpServer) {
            await this.restart();
        }
    }

    async dispose(): Promise<void> {
        if (this.mcpServerDisposable) {
            this.mcpServerDisposable.dispose();
            this.mcpServerDisposable = null;
        }
        
        this.didChangeEmitter.dispose();
        
        if (this.mcpServer) {
            await this.stopMcpServer();
        }
    }

    isRunning(): boolean {
        return this.mcpServer !== null;
    }

    async restart(): Promise<void> {
        if (this.mcpServer) {
            await this.stopMcpServer();
        }
        
        if (this.isMcpEnabled() && this.isMcpApiAvailable()) {
            await this.startMcpServer();
            await this.registerServer();
        }
    }
    
    /**
     * Check if MCP API is available in current VS Code version
     */
    private isMcpApiAvailable(): boolean {
        return typeof vscode.lm?.registerMcpServerDefinitionProvider === 'function';
    }

    /**
     * Public method to check if MCP is supported in current environment
     */
    public isMcpSupported(): boolean {
        return this.isMcpApiAvailable();
    }

    /**
     * Get MCP status information
     */
    public getMcpStatus(): { supported: boolean; enabled: boolean; running: boolean; apiAvailable: boolean } {
        return {
            supported: this.isMcpApiAvailable(),
            enabled: this.isMcpEnabled(),
            running: this.isRunning(),
            apiAvailable: this.isMcpApiAvailable()
        };
    }

    /**
     * Register the MCP server with VS Code
     */
    async registerServer(): Promise<void> {
        // Check if MCP API is available
        if (!this.isMcpApiAvailable()) {
            console.warn('MCP API is not available in this VS Code version. MCP server registration skipped.');
            vscode.window.showWarningMessage(
                'MCP functionality requires a newer version of VS Code. Please update VS Code to use MCP features.',
                'Learn More'
            ).then(selection => {
                if (selection === 'Learn More') {
                    vscode.env.openExternal(vscode.Uri.parse('https://code.visualstudio.com/updates'));
                }
            });
            return;
        }

        try {
            this.mcpServerDisposable = vscode.lm.registerMcpServerDefinitionProvider('pascal-mcp-server', {
                onDidChangeMcpServerDefinitions: this.didChangeEmitter.event,
                provideMcpServerDefinitions: async () => {
                    let servers: vscode.McpServerDefinition[] = [];

                    // Only register when server is running
                    if (this.mcpServer && this.mcpServer.isRunning()) {
                        const serverStatus = this.mcpServer.getServerStatus();
                        if (serverStatus.mcpUrl) {
                            servers.push(new vscode.McpHttpServerDefinition(
                                'pascal-mcp-server',
                                vscode.Uri.parse(serverStatus.mcpUrl),
                                {
                                    'Content-Type': 'application/json',
                                    'User-Agent': 'FPCToolkit-Extension'
                                },
                                "1.0.0"
                            ));
                        }
                    }

                    return servers;
                },
                resolveMcpServerDefinition: async (server: vscode.McpServerDefinition) => {
                    if (server.label === 'pascal-mcp-server') {
                        // Ensure server is running
                        if (!this.mcpServer || !this.mcpServer.isRunning()) {
                            throw new Error('Pascal MCP Server is not running');
                        }
                    }
                    return server;
                }
            });
            
            this.context.subscriptions.push(this.mcpServerDisposable);
        } catch (error) {
            console.error('Failed to register MCP server definition provider:', error);
            vscode.window.showErrorMessage(`Failed to register MCP server: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }
    
    /**
     * Manually register the MCP server with external applications
     * This can be called from a command to force registration
     */
    private async registerMcpServer(): Promise<void> {
        if (!this.mcpServer) {
            vscode.window.showWarningMessage('Pascal MCP Server is not running. Start it first before registering.');
            return;
        }
        
        await this.registerServer();
    }
}
