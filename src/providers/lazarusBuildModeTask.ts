import * as vscode from 'vscode';
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
        }
    }
    
    /**
     * Get compile options for this Lazarus build mode
     * @param workspaceRoot Workspace root path
     * @returns CompileOption object
     */
    getCompileOption(workspaceRoot: string): CompileOption {
        // 创建任务定义
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
    private createTaskDefinition(workspaceRoot: string): FpcTaskDefinition {
        // Create task definition
        const taskDef = new FpcTaskDefinition();
        const mainFile = (this.project as LazarusProject).mainFile || '';
        taskDef.file = LazarusVariableSubstitution.substitute(mainFile);
        
        // Set working directory as project directory
        taskDef.cwd = workspaceRoot;
        
        // Initialize variable substitution system
        const projectFile = (this.project as LazarusProject).mainFile || '';
        const projectName = projectFile.replace(/\.(lpr|lpi)$/i, '');
        
        LazarusVariableSubstitution.initialize(
            null, // We don't need full buildMode object since we have specific targetOS/targetCPU
            workspaceRoot,
            projectName,
            projectFile,
            this.targetFile, // Use target file extracted from build mode
            this.outputDirectory
        );
        
        // Manually set target platform variables
        if (this.targetOS) {
            LazarusVariableSubstitution.setVariable('TargetOS', this.targetOS);
        }
        if (this.targetCPU) {
            LazarusVariableSubstitution.setVariable('TargetCPU', this.targetCPU);
        }
        
        // Create build option
        const buildOption = new BuildOption();
        
        // Fill BuildOption from detailed build options
        if (this.detailedBuildOptions) {
            // Copy all detailed options to BuildOption (except those converted to customOptions)
            Object.assign(buildOption, this.detailedBuildOptions);
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
        
        // Add include paths to compiler options (using -Fi parameter)
        if (this.includePaths && this.includePaths.length > 0) {
            const includeOptions = this.includePaths.map(path => 
                `-Fi${LazarusVariableSubstitution.substitute(path)}`
            );
            buildOption.customOptions = (buildOption.customOptions || []).concat(includeOptions);
        }
        
        // Add search paths and perform variable substitution
        if (this.unitPaths && this.unitPaths.length > 0) {
            buildOption.searchPath = this.unitPaths.map(path => 
                LazarusVariableSubstitution.substitute(path)
            );
        }
        
        // Add library paths and perform variable substitution
        if (this.libraryPaths && this.libraryPaths.length > 0) {
            buildOption.libPath = this.libraryPaths.map(path => 
                LazarusVariableSubstitution.substitute(path)
            );
        }
        
        // Add output directory and perform variable substitution
        if (this.outputDirectory) {
            buildOption.unitOutputDir = LazarusVariableSubstitution.substitute(this.outputDirectory);
        }
        
        // If object path exists, set as output directory and perform variable substitution
        if (this.objectPath) {
            buildOption.unitOutputDir = LazarusVariableSubstitution.substitute(this.objectPath);
        }
        
        // Add target platform (prefer value from detailed options)
        if (buildOption.targetOS || this.targetOS) {
            buildOption.targetOS = buildOption.targetOS || this.targetOS;
        }
        
        if (buildOption.targetCPU || this.targetCPU) {
            buildOption.targetCPU = buildOption.targetCPU || this.targetCPU;
        }
        
        // Set output file name (from Lazarus project <Target><Filename Value="..."/>)
        if (this.targetFile) {
            buildOption.outputFile = LazarusVariableSubstitution.substitute(this.targetFile);
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
    setAsDefault(): void {
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
            const tasks = config.tasks || [];
            let tasksUpdated = false;
            
            for (let i = 0; i < tasks.length; i++) {
                if (tasks[i].type === 'fpc' && typeof tasks[i].group === 'object' && tasks[i].group.isDefault) {
                    tasks[i].group.isDefault = undefined;
                    tasksUpdated = true;
                }
            }
            
            // If any tasks updated, save tasks.json
            if (tasksUpdated) {
                config.update(
                    "tasks",
                    tasks,
                    vscode.ConfigurationTarget.WorkspaceFolder
                ).then(() => {
                    console.log('Cleared default status for all FPC tasks');
                }, (error) => {
                    console.error('Error updating tasks.json:', error);
                });
            }
            
      
        } catch (error) {
            console.error(`Error updating tasks.json:`, error);
        }
    }
}