import * as vscode from 'vscode';
import * as path from 'path';
import { CompileOption } from '../languageServer/options';
import { IProjectIntf, IProjectTask } from './projectIntf';
import { LazarusProject } from './lazarus';
import { DefaultBuildModeStorage } from './defaultBuildModeStorage';
import { FpcTaskDefinition, BuildOption } from './task';
import { LazarusVariableSubstitution } from './lazarusVariables';

/**
 * Lazarus build mode implements IProjectTask
 * Represents a build mode in a Lazarus project
 */
export class LazarusBuildModeTask implements IProjectTask {
    // Build mode properties
    id?: string;             // Build mode identifier
    name?: string;           // Build mode name
    buildMode?: string;      // Current build mode name
    targetOS?: string;       // Target OS for current build mode
    targetCPU?: string;      // Target CPU for current build mode
    targetFile?: string;     // Target file name (from <Target><Filename Value="..."/>)
    compilerOptions?: string[]; // Compiler options
    unitPaths?: string[];    // Unit paths
    includePaths?: string[]; // Include file paths
    libraryPaths?: string[]; // Library paths
    objectPath?: string;     // Object file output path
    outputDirectory?: string; // Output directory for current build mode
    isInLpi: boolean; // Whether defined in LPI file
    useLCL: boolean = false; // Whether to use LCL (Lazarus Component Library)
    // Detailed build options - raw data parsed directly from LPI file
    detailedBuildOptions?: {
        // Debugging options
        debugInfoType?: string;
        useHeaptrc?: boolean;
        trashVariables?: boolean;
        useExternalDbgSyms?: boolean;
        generateDebugInfo?: boolean;
        
        // Code generation options  
        ioChecks?: boolean;
        rangeChecks?: boolean;
        overflowChecks?: boolean;
        stackChecks?: boolean;
        smartLinkUnit?: boolean;
        linkSmart?: boolean;
        verifyObjMethodCallValidity?: boolean;
        optimizationLevel?: number;
        
        // Parsing options
        includeAssertionCode?: boolean;
        
        // Custom options
        customDefines?: string[];
        conditionalDefines?: string[];
        customOptions?: string[];  // Raw custom compiler options
        
        syntaxMode?: string; // Syntax mode
        // Other raw options
        [key: string]: any;
    };
    
    // IProjectTask implementation
    label: string;           // Display name
    project: IProjectIntf;   // Associated project interface
    
    constructor(
        modelName: string,
        isDefault: boolean,
        isInLpi: boolean,
        project: IProjectIntf,
        buildMode?: string,
        targetOS?: string,
        targetCPU?: string,
        targetFile?: string
    ) {
        this.buildMode = buildMode || modelName;
        this.project = project;
        this.targetOS = targetOS;
        this.targetCPU = targetCPU;
        this.targetFile = targetFile;
        this.isInLpi = isInLpi;
        this.label = modelName;

        // Generate id: [file]-[label]-[targetOS]-[targetCPU]
        this.id = `${this.project.file}-${this.label}-${this.targetOS||''}-${this.targetCPU || ''}`;
        
        // If default build mode, set as default
        if (isDefault) {
            this.setAsDefault();
        }
    }
    
    /**
     * Get whether this build mode is default
     */
    get isDefault(): boolean {
        // Get default status from global storage
        const storage = DefaultBuildModeStorage.getInstance();
        return storage.isDefaultBuildMode(this.id || '');
    }
    
    /**
     * Set whether this build mode is default
     */
    set isDefault(value: boolean) {
        if (value) {
            this.setAsDefault();
        } else {
            // If setting to false, only clear if it is currently the default
            const storage = DefaultBuildModeStorage.getInstance();
            if (storage.isDefaultBuildMode(this.id || '')) {
                storage.setDefaultBuildMode("");
                console.log(`Cleared Lazarus default build mode ${this.buildMode}`);
            }
        }
    }
    
    /**
     * Get compile options for this Lazarus build mode
     * @param workspaceRoot Workspace root path
     * @returns CompileOption object
     */
    getCompileOption(workspaceRoot: string): CompileOption {
        // Create task definition
        const taskDef = this.createTaskDefinition(workspaceRoot);
        return new CompileOption(taskDef, workspaceRoot);
    }
    
    /**
     * Convert detailedBuildOptions to customOptions array
     * @returns Custom compiler options array
     */
    private convertDetailedBuildOptionsToCustomOptions(): string[] {
        const customOptions: string[] = [];
        
        if (!this.detailedBuildOptions) {
            return ['-B']; // Default forced rebuild
        }
        
        // === Debug options ===
        if (this.detailedBuildOptions.debugInfoType) {
            switch (this.detailedBuildOptions.debugInfoType.toLowerCase()) {
                case 'dsdwarf2':
                case 'dsdwarf2set':
                    customOptions.push('-gw2');
                    break;
                case 'dsdwarf3':
                    customOptions.push('-gw3');
                    break;
                case 'dsdwarf4':
                    customOptions.push('-gw4');
                    break;
                case 'dsstabs':
                    customOptions.push('-gs');
                    break;
                case 'dsauto':
                    customOptions.push('-g');
                    break;
                default:
                    customOptions.push('-g');
                    break;
            }
        }
        
        if (this.detailedBuildOptions.generateDebugInfo && !this.detailedBuildOptions.debugInfoType) {
            customOptions.push('-g');
        }
        
        if (this.detailedBuildOptions.useHeaptrc) {
            customOptions.push('-gh');
        }
        
        if (this.detailedBuildOptions.trashVariables) {
            customOptions.push('-gt');
        }
        
        if (this.detailedBuildOptions.useExternalDbgSyms) {
            customOptions.push('-Xg');
        }
        
        // === Code generation options ===
        if (this.detailedBuildOptions.ioChecks) {
            customOptions.push('-Ci');
        }
        
        if (this.detailedBuildOptions.rangeChecks) {
            customOptions.push('-Cr');
        }
        
        if (this.detailedBuildOptions.overflowChecks) {
            customOptions.push('-Co');
        }
        
        if (this.detailedBuildOptions.stackChecks) {
            customOptions.push('-Ct');
        }
        
        if (this.detailedBuildOptions.smartLinkUnit) {
            customOptions.push('-CX');
        }
        
        if (this.detailedBuildOptions.linkSmart) {
            customOptions.push('-XX');
        }
        
        if (this.detailedBuildOptions.verifyObjMethodCallValidity) {
            customOptions.push('-CR');
        }
        
        // === Parsing options ===
        if (this.detailedBuildOptions.includeAssertionCode) {
            customOptions.push('-Sa');
        }
        
        // === Custom defines ===
        if (this.detailedBuildOptions.customDefines && this.detailedBuildOptions.customDefines.length > 0) {
            this.detailedBuildOptions.customDefines.forEach(define => {
                if (define.trim()) {
                    customOptions.push(`-d${define.trim()}`);
                }
            });
        }
        
        if (this.detailedBuildOptions.conditionalDefines && this.detailedBuildOptions.conditionalDefines.length > 0) {
            this.detailedBuildOptions.conditionalDefines.forEach(define => {
                if (define.trim()) {
                    customOptions.push(`-d${define.trim()}`);
                }
            });
        }
        
        // === Raw custom options ===
        if (this.detailedBuildOptions.customOptions && this.detailedBuildOptions.customOptions.length > 0) {
            customOptions.push(...this.detailedBuildOptions.customOptions);
        }
        
        // If no options, add basic option
        if (customOptions.length === 0) {
            customOptions.push('-B');
        }
        
        return customOptions;
    }
    
    /**
     * Create task definition for this build mode
     * @param workspaceRoot Workspace root path
     * @returns FpcTaskDefinition object
     */
    public createTaskDefinition(workspaceRoot: string): FpcTaskDefinition {
        // Create task definition
        const taskDef = new FpcTaskDefinition();
        const projectDir = path.dirname(this.project.file);
        const projectFile = path.basename(this.project.file);
        const projectName = projectFile.replace(/\.(lpr|lpi)$/i, '');

        // Initialize variable substitution system with proper project directory
        const buildModeForSubstitution = {
            targetOS: this.targetOS,
            targetCPU: this.targetCPU,
            name: this.buildMode
        };
        
        LazarusVariableSubstitution.initialize(
            buildModeForSubstitution as any, 
            projectDir,
            projectName,
            projectFile,
            this.targetFile,
            this.outputDirectory
        );

        // Helper to resolve variables and relativize to workspace root
        const resolveAndRelativize = (input: string) => {
            if (!input) return input;
            const substituted = LazarusVariableSubstitution.substitute(input);
            const absolute = path.isAbsolute(substituted) ? substituted : path.resolve(projectDir, substituted);
            return path.relative(workspaceRoot, absolute);
        };

        const mainFile = (this.project as LazarusProject).mainFile || '';
        taskDef.file = resolveAndRelativize(mainFile);
        
        // Set working directory default to workspace root (handled by task provider)
        
        // Create build option
        const buildOption = new BuildOption();
        
        // Explicitly copy supported BuildOption fields from detailed build options
        if (this.detailedBuildOptions) {
            const supportedFields: (keyof BuildOption)[] = [
                'targetOS', 'targetCPU', 'optimizationLevel', 
                'syntaxMode', 'forceRebuild', 'msgIgnore'
            ];
            
            supportedFields.forEach(field => {
                if ((this.detailedBuildOptions as any)[field] !== undefined) {
                    (buildOption as any)[field] = (this.detailedBuildOptions as any)[field];
                }
            });
        }
        
        // Convert detailed build options to custom compiler options
        const detailedCustomOptions = this.convertDetailedBuildOptionsToCustomOptions();
        
        // Add compiler options and perform variable substitution
        if (this.compilerOptions && this.compilerOptions.length > 0) {
            // If traditional compiler options exist, use them first
            buildOption.customOptions = this.compilerOptions.map(option => 
                LazarusVariableSubstitution.substitute(option)
            );
        } else {
            // Otherwise use options converted from detailed build options
            buildOption.customOptions = detailedCustomOptions.map(option => 
                LazarusVariableSubstitution.substitute(option)
            );
        }
        
        // Add include paths
        if (this.includePaths && this.includePaths.length > 0) {
            buildOption.includePath = this.includePaths.map(resolveAndRelativize);
        } else {
            buildOption.includePath = [];
        }
        
        // Add include paths from required packages
        const lazProject = this.project as LazarusProject;
        if (lazProject.packageIncludePaths && lazProject.packageIncludePaths.length > 0) {
            const pkgIncPaths = lazProject.packageIncludePaths.map(resolveAndRelativize);
            buildOption.includePath = (buildOption.includePath || []).concat(pkgIncPaths);
        }
        
        // Add search paths and perform variable substitution
        if (this.unitPaths && this.unitPaths.length > 0) {
            buildOption.searchPath = this.unitPaths.map(resolveAndRelativize);
        } else {
            buildOption.searchPath = [];
        }
        
        // Ensure the project directory itself is in the search path
        // This helps FPC find the main .lpr file when executing from workspace root
        const relProjDir = path.relative(workspaceRoot, projectDir) || '.';
        if (!buildOption.searchPath.includes(relProjDir)) {
            buildOption.searchPath.unshift(relProjDir);
        }

        // Add search paths from required packages
        if (lazProject.packageSearchPaths && lazProject.packageSearchPaths.length > 0) {
            const pkgPaths = lazProject.packageSearchPaths.map(resolveAndRelativize);
            buildOption.searchPath = (buildOption.searchPath || []).concat(pkgPaths);
        }
        
        // Add library paths and perform variable substitution
        if (this.libraryPaths && this.libraryPaths.length > 0) {
            buildOption.libPath = this.libraryPaths.map(resolveAndRelativize);
        }
        
        // Handle output directory
        let outDir = this.outputDirectory || this.objectPath;
        if (outDir) {
            buildOption.unitOutputDir = resolveAndRelativize(outDir);
        }
        
        // Add target platform
        buildOption.targetOS = buildOption.targetOS || this.targetOS;
        buildOption.targetCPU = buildOption.targetCPU || this.targetCPU;
        
        // Set output file name
        if (this.targetFile) {
            buildOption.outputFile = resolveAndRelativize(this.targetFile);
        } 
        if(this.useLCL){
            buildOption?.customOptions?.push('-dLCL');
            if(this.targetOS === 'win32' || this.targetOS === 'win64') {
                buildOption?.customOptions?.push('-dLCLWin');
            }
        }
        
        // Set build option to task definition
        taskDef.buildOption = buildOption;
        
        return taskDef;
    }
    
    /**
     * Get tree item for display in TreeDataProvider
     * @returns TreeItem for this Lazarus build mode
     */
    getTreeItem(): vscode.TreeItem {
        // Use already built label (includes modelname-targetOs-targetCpu)
        const displayLabel = this.label;

        // Create tree item
        const item = new vscode.TreeItem(displayLabel, vscode.TreeItemCollapsibleState.None);
        item.contextValue = 'lazarusbuildmode';
        item.tooltip = `${displayLabel} (${this.project.file})`;
        
        // Add description for default build mode
        if (this.isDefault) {
            item.description = 'default';
        }
        
        return item;
    }
    
    /**
     * Get vscode.Task object for this build mode
     * @returns vscode.Task object
     */
    getTask(): vscode.Task {
        // Get workspace root directory
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath || '';
        
        // Create task definition
        const taskDef = this.createTaskDefinition(workspaceRoot);
        
        // Add build mode information to task definition for Lazarus compilation
        (taskDef as any).buildMode = this.buildMode;
        (taskDef as any).isLazarusProject = true;
        (taskDef as any).lazarusProjectFile = this.project.file;
        
        // Get task from taskProvider
        const { taskProvider } = require('./task');
        return taskProvider.getTask(
            this.label,
            (this.project as LazarusProject).mainFile || '',
            taskDef
        );
    }
    
    /**
     * Set this build mode as default
     * Use global persistent storage to manage default status
     */
    async setAsDefault(): Promise<void> {
        // Get global storage
        const storage = DefaultBuildModeStorage.getInstance();
        
        // Set this build mode as default
        storage.setDefaultBuildMode(this.id || '');
        
        console.log(`Set Lazarus build mode ${this.buildMode} as default`);
        
        try {
            // Get workspace root directory
            const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
            if (!workspaceRoot) {
                console.error('No workspace folder found');
                return;
            }
            
            // Clear default flag for all FPC tasks in tasks.json
            const config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(workspaceRoot));
            const tasks = config.get<any[]>('tasks') || [];
            let tasksUpdated = false;
            
            for (let i = 0; i < tasks.length; i++) {
                if (tasks[i].type === 'fpc' && typeof tasks[i].group === 'object' && tasks[i].group.isDefault) {
                    tasks[i].group.isDefault = undefined;
                    tasksUpdated = true;
                }
            }
            
            // If any tasks updated, save tasks.json
            if (tasksUpdated) {
                await config.update(
                    "tasks",
                    tasks,
                    vscode.ConfigurationTarget.WorkspaceFolder
                );
                console.log('Cleared default status for all FPC tasks');
            }
            
      
        } catch (error) {
            console.error(`Error updating tasks.json:`, error);
        }
    }
}