
import * as vscode from 'vscode';
import * as path from 'path';

/**
 * ProjectScanner handles unified workspace scanning for project files.
 */
export class ProjectScanner {
    private static instance: ProjectScanner;
    private _onDidScan = new vscode.EventEmitter<vscode.Uri[]>();
    readonly onDidScan = this._onDidScan.event;

    private _cachedFiles: vscode.Uri[] = [];
    private _isScanning = false;

    private constructor() {}

    static getInstance(): ProjectScanner {
        if (!ProjectScanner.instance) {
            ProjectScanner.instance = new ProjectScanner();
        }
        return ProjectScanner.instance;
    }

    async scan(force = false): Promise<vscode.Uri[]> {
        if (this._isScanning) return this._cachedFiles;
        if (this._cachedFiles.length > 0 && !force) return this._cachedFiles;

        this._isScanning = true;
        try {
            // Centralized scan for all relevant project file extensions
            const files = await vscode.workspace.findFiles("**/*.{lpr,dpr,lpi,lpk}", null);
            this._cachedFiles = files;
            this._onDidScan.fire(files);
            
            // Set context for UI
            const hasLpi = files.some(f => f.fsPath.toLowerCase().endsWith('.lpi'));
            const hasFpc = files.some(f => f.fsPath.toLowerCase().endsWith('.lpr') || f.fsPath.toLowerCase().endsWith('.dpr'));
            vscode.commands.executeCommand('setContext', 'fpctoolkit.lazarus.hasProjects', hasLpi);
            vscode.commands.executeCommand('setContext', 'fpctoolkit.fpc.hasProjects', hasFpc);
            
            return files;
        } finally {
            this._isScanning = false;
        }
    }

    get cachedFiles(): vscode.Uri[] {
        return this._cachedFiles;
    }

    clearCache() {
        this._cachedFiles = [];
    }
}
