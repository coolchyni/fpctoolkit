import * as vscode from 'vscode';
import { FpcProjectProvider } from './providers/project';
import { FpcItem } from './providers/fpcItem';
import { ProjectType } from './providers/projectType';
import { diagCollection, FpcTaskProvider, taskProvider, FpcTask, BuildMode } from './providers/task';
import { FpcCommandManager } from './commands';
import * as util from './common/util';
import { TLangClient } from './languageServer/client';
import { configuration } from './common/configuration';
import { JediFormatter } from './formatter';
import * as MyCodeAction from './languageServer/codeaction';
import * as path from 'path';
import * as fs from 'fs';
import { McpManager } from './mcp';
import { version } from 'os';
export let client: TLangClient;
export let formatter: JediFormatter;
export let logger: vscode.OutputChannel;
export let projectProvider: FpcProjectProvider;
export let commandManager: FpcCommandManager;
export let mcpManager: McpManager;

// Check file updates and auto-compile before debugging
async function checkAndBuildBeforeDebug(): Promise<void> {
    try {
        // Get workspace root directory
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        if (!workspaceRoot) {
            logger.appendLine('Debug pre-check: No workspace folder found');
            return;
        }

        // Check if auto-build feature is enabled
        const config = vscode.workspace.getConfiguration('fpctoolkit');
        const autoBuildEnabled = config.get<boolean>('debug.autoBuild', true);

        if (!autoBuildEnabled) {
            logger.appendLine('Debug pre-check: Auto-build feature is disabled');
            return;
        }

        if (!projectProvider) {
            console.log('Project provider not initialized');
            return;
        }

        // Use file system watcher to check if files have changed
        if (!projectProvider.hasSourceFileChanged()) {
            //logger.appendLine('Debug pre-check: No source file changes, skipping compilation');
            return;
        }

        logger.appendLine('Debug pre-check: Source file changes detected, compilation needed');

        // Get default project and execute compilation
        const defaultFpcItem = await projectProvider.ensureDefaultFpcItem();

        if (!defaultFpcItem) {
            // Refresh project list, try to get default project
            await new Promise<void>((resolve) => {
                const disposable = projectProvider.onDidChangeTreeData(() => {
                    disposable.dispose();
                    resolve();
                });
                projectProvider.refresh();
            });

            // Try to get default project again
            const defaultFpcItem = await projectProvider.ensureDefaultFpcItem();
            if (!defaultFpcItem) {
                logger.appendLine('Debug pre-check: No default project found');
                return;
            }
        }

        let defaultTask = defaultFpcItem?.projectTask?.getTask();

        if (!defaultTask) {
            logger.appendLine('Debug pre-check: No suitable task found for compilation');
            return;
        }

        // If compilation is needed, execute compilation task
        //logger.show(true); // Show output window
        logger.appendLine('Debug auto-compilation: File changes detected, starting compilation');

        try {

            // Execute task
            const execution = await vscode.tasks.executeTask(defaultTask);

            // Wait for task completion
            await new Promise<void>((resolve, reject) => {
                let taskCompleted = false;

                // Listen for task process end to get exit code
                const processDisposable = vscode.tasks.onDidEndTaskProcess(async (e) => {
                    if (e.execution === execution && !taskCompleted) {
                        taskCompleted = true;
                        processDisposable.dispose();
                        taskDisposable.dispose();

                        // Check if task executed successfully
                        if (e.exitCode === 0) {
                            // Reset source file change flag only on successful compilation
                            projectProvider.resetSourceFileChanged();
                            logger.appendLine('Debug auto-compilation: Compilation completed successfully');
                            resolve();
                        } else {
                            logger.appendLine(`Debug auto-compilation: Compilation failed with exit code ${e.exitCode}`);
                            // Show user prompt asking whether to continue debugging despite compilation failure
                            const choice = await vscode.window.showWarningMessage(
                                `Compilation failed with exit code ${e.exitCode}. Do you want to continue debugging anyway?`,
                                'Continue',
                                'Cancel'
                            );

                            if (choice === 'Continue') {
                                logger.appendLine('Debug auto-compilation: User chose to continue debugging despite compilation failure');
                                resolve();
                            } else {
                                logger.appendLine('Debug auto-compilation: User cancelled debugging due to compilation failure');
                                reject(new Error(`Compilation failed with exit code ${e.exitCode}`));
                            }
                        }
                    }
                });

                // Listen for task end as fallback (in case process end doesn't fire)
                const taskDisposable = vscode.tasks.onDidEndTask((e) => {
                    if (e.execution === execution && !taskCompleted) {
                        taskCompleted = true;
                        processDisposable.dispose();
                        taskDisposable.dispose();

                        // If we only get task end without process end, assume success
                        projectProvider.resetSourceFileChanged();
                        logger.appendLine('Debug auto-compilation: Compilation completed (exit code unknown)');
                        resolve();
                    }
                });

                // Set timeout to avoid infinite waiting
                setTimeout(() => {
                    if (!taskCompleted) {
                        taskCompleted = true;
                        processDisposable.dispose();
                        taskDisposable.dispose();
                        logger.appendLine('Debug auto-compilation: Compilation timeout');
                        reject(new Error('Compilation timeout'));
                    }
                }, 30000); // 30 second timeout
            });

        } catch (buildError) {
            logger.appendLine(`Debug auto-compilation: Compilation failed - ${buildError}`);
            throw buildError;
        }
    } catch (error) {
        const errorMsg = `Error occurred during auto-compilation: ${error}`;
        console.error(errorMsg);
        logger.appendLine(errorMsg);

        // Don't show error message here, let the caller handle it
        // Re-throw the error so resolveDebugConfiguration can catch it and cancel debugging
        throw error;
    }
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

    if (!vscode.workspace.workspaceFolders) {
        return;
    }

    // Register debug configuration provider to check and compile project before debugging starts
    vscode.debug.registerDebugConfigurationProvider('*', {
        resolveDebugConfiguration: (folder, config, token) => {
            // Just return config, don't do compilation here to avoid task.json redirect
            return config;
        },
        resolveDebugConfigurationWithSubstitutedVariables: async (folder, config, token) => {
            try {
                // Check for file updates before debugging starts and auto-compile default project if needed
                await checkAndBuildBeforeDebug();
                return config; // Return config to continue debugging
            } catch (error) {
                // If checkAndBuildBeforeDebug throws an error (user cancelled or compilation failed), 
                // return undefined to cancel the debug session without opening task.json
                logger.appendLine(`Debug session cancelled: ${error}`);
                return undefined; // This will cancel the debug session without opening config
            }
        }
    });

    // Register debug session start events (keep original logic)
    vscode.debug.onDidReceiveDebugSessionCustomEvent((event) => {
        console.log('Custom event received:', event);
    });
    vscode.debug.onDidStartDebugSession(async (session) => {
        console.log('Debug session started:', session.name);
    });

    logger = vscode.window.createOutputChannel('fpctoolkit');

    vscode.window.onDidChangeVisibleTextEditors(onDidChangeVisibleTextEditors);
    vscode.workspace.onDidChangeTextDocument(onDidChangeTextDocument);
    util.setExtensionContext(context);
    const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;


    commandManager = new FpcCommandManager(workspaceRoot);
    commandManager.registerAll(context);

    // Set extension context
    FpcCommandManager.setContext(context);

    formatter = new JediFormatter();
    formatter.doInit();

    projectProvider = new FpcProjectProvider(workspaceRoot, context);
    vscode.window.registerTreeDataProvider("FpcProjectExplorer", projectProvider);

    //taskProvider=new FpcTaskProvider(workspaceRoot);
    context.subscriptions.push(vscode.tasks.registerTaskProvider(
        FpcTaskProvider.FpcTaskType,
        taskProvider
    )
    );

    // Initialize MCP Manager - only if MCP support is enabled and API is available
    const config = vscode.workspace.getConfiguration('fpctoolkit');
    const mcpEnabled = config.get<boolean>('mcp.enabled', true);
    if (mcpEnabled) {
        try {
            mcpManager = new McpManager(context, workspaceRoot);
            await mcpManager.initialize(projectProvider);
        } catch (error) {
            console.error('Failed to initialize MCP Manager:', error);
            logger.appendLine(`MCP initialization failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }

    MyCodeAction.activate(context);

    // Listen for configuration changes to handle dynamic enabling/disabling of features
    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(async (event) => {
            // Handle Lazarus support configuration changes
            if (event.affectsConfiguration('fpctoolkit.lazarus.enabled')) {
                // Refresh project provider to show/hide Lazarus projects
                if (projectProvider) {
                    projectProvider.refresh();
                }
            }

            // Handle MCP configuration changes
            if (event.affectsConfiguration('fpctoolkit.mcp.enabled')) {
                const newMcpEnabled = vscode.workspace.getConfiguration('fpctoolkit').get<boolean>('mcp.enabled', true);

                if (newMcpEnabled && !mcpManager) {
                    // MCP was enabled, initialize it
                    try {
                        mcpManager = new McpManager(context, workspaceRoot);
                        await mcpManager.initialize(projectProvider);
                    } catch (error) {
                        console.error('Failed to initialize MCP Manager:', error);
                        logger.appendLine(`MCP initialization failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
                    }
                } else if (!newMcpEnabled && mcpManager) {
                    // MCP was disabled, dispose it
                    try {
                        await mcpManager.dispose();
                        mcpManager = undefined as any;
                    } catch (error) {
                        console.error('Failed to dispose MCP Manager:', error);
                        logger.appendLine(`MCP disposal failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
                    }
                }
            }
        })
    );

    client = new TLangClient(projectProvider);
    await client.doInit();
    client.start();

}

function onDidChangeTextDocument(e: vscode.TextDocumentChangeEvent) {
    if (e.contentChanges.length > 0) {
        if (!diagCollection.has(e.document.uri)) { return; }


        for (const change of e.contentChanges) {
            let newline = (change.text.match(/\n/g) || '').length + 1;
            if (change.range.isSingleLine && (newline < 2)) {
                continue;
            }
            let diags = diagCollection.get(e.document.uri);
            if (!diags) { return; }

            let oldline = change.range.end.line - change.range.start.line + 1;

            let lines_change = newline - oldline;
            let newdiags = [];
            for (const diag of diags) {
                if (change.range.contains(diag.range)) {//remove it if contains
                    continue;
                }

                if (diag.range.start.line >= change.range.start.line) {
                    diag.range = new vscode.Range(diag.range.start.line + lines_change, diag.range.start.character, diag.range.end.line + lines_change, diag.range.end.character)
                }
                newdiags.push(diag)
            }
            diagCollection.set(e.document.uri, newdiags);
        }



    }

}
function onDidChangeVisibleTextEditors(editors: readonly vscode.TextEditor[]): void {
    // Process delayed didOpen for any visible editors we haven't seen before
    editors.forEach(editor => {
        if ((editor.document.uri.scheme === "file") && (editor.document.languageId === "objectpascal" || editor.document.languageId === "pascal")) {
            editor.options.tabSize = configuration.get<number>('format.tabsize', 2);
            client.onDidChangeVisibleTextEditor(editor);
        }
    });
}
// this method is called when your extension is deactivated
export async function deactivate() {
    // Dispose MCP manager if it was initialized
    if (mcpManager) {
        await mcpManager.dispose();
    }
    // Stop language client if it was initialized
    if (!client) {
        return undefined;
    }
    return client.stop();
}
