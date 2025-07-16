import * as vscode from 'vscode';
import { CompileOption } from '../languageServer/options';
import { IProjectIntf, IProjectTask } from './projectIntf';
import { DefaultBuildModeStorage } from './defaultBuildModeStorage';
/**
 * FPC Task implementation of IProjectTask
 * Represents a task defined in tasks.json
 */
export class FpcTask implements IProjectTask {
    isInLpi: boolean = false;
    constructor(
        public label: string,
        public isDefault: boolean,
        public project: IProjectIntf,
        private taskDefinition: any
    ) {}

    /**
     * Get compile options for this FPC task
     * @param workspaceRoot Workspace root path
     * @returns CompileOption object
     */
    getCompileOption(workspaceRoot: string): CompileOption {
        return new CompileOption(this.taskDefinition, workspaceRoot);
    }

    /**
     * Get tree item for display in TreeDataProvider
     * @returns TreeItem for this FPC task
     */
    getTreeItem(): vscode.TreeItem {
        // Create display label based on task properties
        let displayLabel = this.label;

        // Add platform info to label if available
        if (this.taskDefinition.targetOS || this.taskDefinition.targetCPU) {
            displayLabel += '-';
            if (this.taskDefinition.targetOS) {
                displayLabel += this.taskDefinition.targetOS;
            }
            if (this.taskDefinition.targetCPU) {
                displayLabel += '-' + this.taskDefinition.targetCPU;
            }
        }

        // Create tree item
        const item = new vscode.TreeItem(displayLabel, vscode.TreeItemCollapsibleState.None);
        item.contextValue = 'fpcbuild';
        item.tooltip = `${displayLabel} (${this.project.file})`;

        // Add description for default task
        if (this.isDefault) {
            item.description = 'default';
        }

        return item;
    }

    /**
     * Get vscode.Task object for this task
     * @returns vscode.Task object
     */
    getTask(): vscode.Task {
        // Get task from taskProvider
        const { taskProvider } = require('./task');
        return taskProvider.getTask(
            this.label,
            this.project.file,
            this.taskDefinition
        );
    }

    /**
     * Set this task as the default task
     * Updates the task.json file to mark this task as default
     */
    setAsDefault(): void {
        this.isDefault = true;
        
        // Clear any Lazarus build mode defaults
        const storage = DefaultBuildModeStorage.getInstance();
        storage.setDefaultBuildMode("");
        
        // Update the task definition if available
        if (this.taskDefinition) {
            if (!this.taskDefinition.group) {
                this.taskDefinition.group = {};
            }
            this.taskDefinition.group.isDefault = true;

            // Update tasks.json file
            try {
                const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
                if (workspaceRoot) {
                    const config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(workspaceRoot));
                    const tasks = config.tasks || [];

                    // Find and update the task in the tasks array
                    for (const task of tasks) {
                        if (task.type === 'fpc') {
                            if (task.label === this.label && task.file === this.project.file) {
                                // Set this task as default
                                if (!task.group) {
                                    task.group = { kind: 'build', isDefault: true };
                                } else {
                                    task.group.isDefault = true;
                                }
                            } else {
                                // Clear default flag from other tasks
                                if (task.group && task.group.isDefault) {
                                    task.group.isDefault = undefined;
                                }
                            }
                        }
                    }

                    // Update the configuration
                    config.update(
                        "tasks",
                        tasks,
                        vscode.ConfigurationTarget.WorkspaceFolder
                    ).then(() => {
                        console.log(`Set FPC task ${this.label} as default`);
                    }, (error) => {
                        console.error(`Error setting FPC task ${this.label} as default:`, error);
                    });
                }
            } catch (error) {
                console.error(`Error setting FPC task ${this.label} as default:`, error);
            }
        }
    }
}

/**
 * FPC Task Project implementation of IProjectIntf
 * Represents a project defined in tasks.json
 */
export class FpcTaskProject implements IProjectIntf {
    // Tasks associated with this project
    public tasks: IProjectTask[] = [];

    constructor(
        public label: string,
        public file: string,
        isDefault: boolean,
        taskDefinitions: any[] | any = []
    ) {
        // Convert single task definition to array if needed
        const taskDefs = Array.isArray(taskDefinitions) ? taskDefinitions : (taskDefinitions ? [taskDefinitions] : []);
        
        // Create task objects for each task definition
        for (const taskDef of taskDefs) {
            if (taskDef) {
                const isTaskDefault = taskDef.group?.isDefault || false;
                this.tasks.push(new FpcTask(
                    taskDef.label || this.label,
                    isTaskDefault,
                    this,
                    taskDef
                ));
            }
        }
        
        // If no tasks were created but we have a file, create a default task
        if (this.tasks.length === 0 && this.file) {
            const defaultTask = new FpcTask(
                "[default]",
                isDefault,
                this,
                { 
                    type: 'fpc',
                    label: this.label,
                    file: this.file
                }
            );
            this.tasks.push(defaultTask);
        }
    }
}