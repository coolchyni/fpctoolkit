import * as vscode from 'vscode';
import * as path from 'path';
import { ProjectType } from './projectType';
import { IProjectIntf, IProjectTask } from './projectIntf';

/**
 * FPC project tree item
 * Used to display FPC and Lazarus projects in the tree view
 * Each project interface corresponds to a task
 */
export class FpcItem extends vscode.TreeItem {
    // Project interface
    public project?: IProjectIntf;
    // Project task
    public projectTask?: IProjectTask;

    constructor(
        public readonly level: number,
        public readonly label: string,
        public readonly collapsibleState: vscode.TreeItemCollapsibleState,
        public readonly file: string,
        public fileexist: boolean,
        public isDefault: boolean,
        public projectType: ProjectType = ProjectType.FPC,
        projectIntfOrTask?: IProjectIntf | IProjectTask  // Can be either a project interface (level=0) or a task (level=1)
    ) {
        // Determine display label based on level
        let displayLabel = label;
        if (level === 0) {
            // Root node (level=0): only show file name
            displayLabel = file;
        }

        super(displayLabel, collapsibleState);

        // Store project interface or task based on level
        if (level === 0 && projectIntfOrTask && 'tasks' in projectIntfOrTask) {
            // This is a project interface (root node)
            this.project = projectIntfOrTask as IProjectIntf;
        } else if (level === 1 && projectIntfOrTask && 'project' in projectIntfOrTask) {
            // This is a project task (child node)
            this.projectTask = projectIntfOrTask as IProjectTask;
            // Also store a reference to the parent project
            this.project = this.projectTask.project;
        }

        // Set context value based on node type
        if (level === 0) {
            this.contextValue = projectType === ProjectType.Lazarus ? 'lazarusproject' : 'fpcproject';
        } else {
            // Add specific context value for build mode nodes
            if (projectType === ProjectType.Lazarus) {
                this.contextValue = 'lazarusbuildmode';
            } else {
                this.contextValue = 'fpcbuild';
            }
        }

        if (this.level > 0) {
            if (this.isDefault) {
                this.description = 'default';
            }
            // Add command to open the file when clicking on the item
            const command = {
                command: "fpctoolkit.project.opensetting",
                title: '',
                arguments: [this]
            };
            this.command = command;
        }

        // Set icon based on project type
        if (this.level === 0) {
            if (projectType === ProjectType.Lazarus) {
                // Lazarus project icon
                this.iconPath = path.join(__dirname, '..', 'images', 'lazarus.png');
            } else {
                // FPC project icon
                this.iconPath = path.join(__dirname, '..', 'images', 'pascal-project.png');
            }
        } else {
            // Build mode or task icon
            if (projectType === ProjectType.Lazarus) {
                this.iconPath = new vscode.ThemeIcon('gear');
            } else {
                this.iconPath = new vscode.ThemeIcon('tools');
            }
        }
    }
}