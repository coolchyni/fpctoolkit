import * as vscode from 'vscode';
import { CompileOption } from '../languageServer/options';


/**
 * Common interface for project information
 * Used by both FpcTaskProject and LazarusProjectInfo
 */
export interface IProjectTask {
    label: string;           // Display name of the task
    isDefault: boolean;      // Whether this is the default task
    isInLpi: boolean;        // Whether this task is defined in the LPI file
    project: IProjectIntf; // Associated project interface
    // Methods
    getTask(): Promise<vscode.Task> | vscode.Task;
    getCompileOption(workspaceRoot: string): CompileOption;  // Get compile options for this project
    getTreeItem(): vscode.TreeItem;                          // Get tree item for display in TreeDataProvider
    setAsDefault(): Promise<void> | void;
}
export interface IProjectIntf {
    // Common properties
    label: string;           // Display name of the project
    file: string;            // Project file path
    tasks?: IProjectTask[];    // Associated task definition
}

