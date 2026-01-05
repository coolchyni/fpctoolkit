import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { CompileOption } from '../languageServer/options';
import { FpcTaskDefinition, FpcTaskProvider, taskProvider } from './task';
import { clearTimeout } from 'timers';
import { LazarusProjectParser, LazarusProject } from './lazarus';
import { IProjectIntf, IProjectTask } from './projectIntf';
import { FpcTask, FpcTaskProject } from './fpcTaskProject';
import { FpcItem } from './fpcItem';
import { ProjectType } from './projectType';
import { LazarusBuildModeTask } from './lazarusBuildModeTask';
import { Message } from 'vscode-languageclient';

export class FpcProjectProvider implements vscode.TreeDataProvider<FpcItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<FpcItem | undefined | void> = new vscode.EventEmitter<FpcItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<FpcItem | undefined | void> = this._onDidChangeTreeData.event;
	private watch!: vscode.FileSystemWatcher;
	private watchlpr!: vscode.FileSystemWatcher;
	private watchlpi!: vscode.FileSystemWatcher; // Monitor Lazarus project files
	private watchSource!: vscode.FileSystemWatcher; // Monitor source file changes
	public defaultFpcItem?: FpcItem = undefined;
	private config!: vscode.WorkspaceConfiguration;
	private defaultCompileOption?: CompileOption = undefined;
	private timeout?: NodeJS.Timeout = undefined;
	private _hasSourceFileChanged: boolean = false; // Flag indicating whether source files have changed
	private _projectInfosMap: Map<string, IProjectIntf> = new Map(); // Store parsed project interfaces
	constructor(private workspaceRoot: string, context: vscode.ExtensionContext, private projectTypeFilter?: ProjectType) {
		const subscriptions = context.subscriptions;
		const name = 'FpcProjectExplorer';

		this.watch = vscode.workspace.createFileSystemWatcher(path.join(workspaceRoot, ".vscode", "tasks.json"), false);
		this.watch.onDidChange(async (url) => {
			taskProvider.clean();
			if (this.timeout != undefined) {
				clearTimeout(this.timeout);
			}
			this.timeout = setTimeout(() => {
				this.checkDefaultAndRefresh();
			}, 1000);
		});
		this.watch.onDidDelete(() => {
			this.refresh();
		});

		this.watchlpr = vscode.workspace.createFileSystemWatcher("**/*.lpr", false, true, false);
		this.watchlpr.onDidCreate(() => {
			this.refresh();
		});
		this.watchlpr.onDidDelete(() => {
			this.refresh();
		});

		// Monitor Lazarus project files
		this.watchlpi = vscode.workspace.createFileSystemWatcher("**/*.lpi", false, false, false);
		this.watchlpi.onDidCreate(() => {
			this._projectInfosMap.clear(); // Clear cache when new project is created
			this.refresh();
		});
		this.watchlpi.onDidDelete(() => {
			this._projectInfosMap.clear(); // Clear cache when project is deleted
			this.refresh();
		});
		this.watchlpi.onDidChange(() => {
			this._projectInfosMap.clear(); // Clear cache when project is modified
			this.refresh();
		});

		// Monitor all Pascal source file changes
		this.watchSource = vscode.workspace.createFileSystemWatcher("**/*.{pas,pp,lpr,inc,p,dpr,dpk,lfm}", false, false, false);
		this.watchSource.onDidChange(() => {
			this._hasSourceFileChanged = true;
		});
		this.watchSource.onDidCreate(() => {
			this._hasSourceFileChanged = true;
		});
		this.watchSource.onDidDelete(() => {
			this._hasSourceFileChanged = true;
		});

	}

	/**
	 * Get cached project info for a LPI file
	 * @param lpiPath Path to the LPI file
	 * @returns Cached project interface or undefined if not cached
	 */
	private getCachedProjectInfos(lpiPath: string): IProjectIntf | undefined {
		return this._projectInfosMap.get(lpiPath);
	}

	/**
	 * Parse and cache project info for a LPI file
	 * @param lpiPath Path to the LPI file
	 * @returns Parsed project interface
	 */
	private parseAndCacheProjectInfos(lpiPath: string): IProjectIntf {
		try {
			const projectIntf = LazarusProjectParser.parseLpiFile(lpiPath);
			if (projectIntf) {
				this._projectInfosMap.set(lpiPath, projectIntf);
				return projectIntf;
			}
		} catch (error) {
			console.error(`Error parsing LPI file ${lpiPath}:`, error);
		}
		// Create a default project info if parsing fails
		const defaultProjectInfo = LazarusProjectParser.createDefaultProjectInfo(lpiPath);
		this._projectInfosMap.set(lpiPath, defaultProjectInfo);
		return defaultProjectInfo;
	}

	/**
	 * Get project info for a LPI file (cached or parsed)
	 * @param lpiPath Path to the LPI file
	 * @returns Project interface
	 */
	public getProjectInfos(lpiPath: string): IProjectIntf {
		// Check cache first
		let projectIntf = this.getCachedProjectInfos(lpiPath);
		if (!projectIntf) {
			// Parse and cache if not found
			projectIntf = this.parseAndCacheProjectInfos(lpiPath);
		}
		return projectIntf;
	}

	/**
	 * Check if source files have changed
	 */
	public hasSourceFileChanged(): boolean {
		return this._hasSourceFileChanged;
	}

	/**
	 * Reset the source file change flag
	 */
	public resetSourceFileChanged(): void {
		this._hasSourceFileChanged = false;
	}

	/**
	 * Ensure we have a default FPC project
	 */
	public async ensureDefaultFpcItem(): Promise<FpcItem | undefined> {
		return this.defaultFpcItem;
	}

	/**
	 * Collect FPC projects from tasks.json
	 * @param itemMaps Project mapping
	 */
	private collectFpcTaskProjects(itemMaps: Map<string, FpcItem>): void {
		if (this.projectTypeFilter !== undefined && this.projectTypeFilter !== ProjectType.FPC) {
			return;
		}
		this.config?.tasks?.forEach((e: any) => {
			if (e.type === 'fpc') {
				const absolutePath = path.isAbsolute(e.file) ? e.file : path.join(this.workspaceRoot, e.file);
				if (!itemMaps.has(absolutePath)) {
					// Create FpcTaskProject as IProjectIntf
					const projectIntf = new FpcTaskProject(
						path.basename(e.file),
						absolutePath,
						e.group?.isDefault || false,
						e
					);

					// Create FpcItem and add to mapping
					itemMaps.set(
						absolutePath,
						new FpcItem(
							0,
							path.basename(e.file),
							vscode.TreeItemCollapsibleState.Expanded,
							absolutePath,
							true,
							e.group?.isDefault || false,
							ProjectType.FPC,
							projectIntf
						)
					);
				} else {
					// If a project with the same file already exists, add task to existing project
					const existingItem = itemMaps.get(absolutePath);
					if (existingItem && existingItem.project) {
						// Add this task to the existing project
						const projectIntf = existingItem.project as FpcTaskProject;
				   let task=new FpcTask(e.label, e.group?.isDefault || false, projectIntf, e);
				   // Ensure isInLpi property for IProjectTask compatibility
				   (task as any).isInLpi = false;
				   projectIntf.tasks.push(task);
						
					}
				}
			}
		});
	}

	/**
	 * Collect projects from workspace files
	 * @param workspaceFolder Workspace folder
	 * @param itemMaps Project mapping
	 */
	private async collectProjectsFromWorkspace(workspaceFolder: vscode.WorkspaceFolder, itemMaps: Map<string, FpcItem>): Promise<void> {
		try {
			// Find all .lpr, .dpr, and .lpi files in the workspace
			// Pass null to exclude to respect files.exclude and .gitignore (if configured)
			const files = await vscode.workspace.findFiles(
				new vscode.RelativePattern(workspaceFolder, "**/*.{lpr,dpr,lpi}"),
				null
			);

			// Update Lazarus project existence context
			const hasLpi = files.some(file => file.fsPath.toLowerCase().endsWith('.lpi'));
			vscode.commands.executeCommand('setContext', 'fpctoolkit.lazarus.hasProjects', hasLpi);

			for (const fileUri of files) {
				const absolutePath = fileUri.fsPath;
				const relativePath = path.relative(workspaceFolder.uri.fsPath, absolutePath);
				
				// Handle .lpr and .dpr files (FPC projects)
				if (absolutePath.toLowerCase().endsWith('.lpr') || absolutePath.toLowerCase().endsWith('.dpr')) {
					if (this.projectTypeFilter === undefined || this.projectTypeFilter === ProjectType.FPC) {
						this.collectFpcProject(absolutePath, itemMaps, workspaceFolder, relativePath);
					}
				}
				// Handle .lpi files (Lazarus projects) - only if Lazarus support is enabled
				else if (absolutePath.toLowerCase().endsWith('.lpi')) {
					if (this.projectTypeFilter === undefined || this.projectTypeFilter === ProjectType.Lazarus) {
						const config = vscode.workspace.getConfiguration('fpctoolkit');
						const lazarusEnabled = config.get<boolean>('lazarus.enabled', true);
						if (lazarusEnabled) {
							this.collectLazarusProject(absolutePath, itemMaps, workspaceFolder, relativePath);
						}
					}
				}
			}
		} catch (error) {
			console.error(`Error collecting projects from workspace ${workspaceFolder.name}:`, error);
		}
	}

	/**
	 * Collect FPC project
	 * @param file Absolute path or file name
	 * @param itemMaps Project mapping
	 * @param workspaceFolder Workspace folder
	 * @param relativePath Optional relative path for display
	 */
	private collectFpcProject(file: string, itemMaps: Map<string, FpcItem>, workspaceFolder: vscode.WorkspaceFolder, relativePath?: string): void {
		try {
			const absolutePath = path.isAbsolute(file) ? file : path.join(workspaceFolder.uri.fsPath, file);
			const displayName = relativePath || file;

			// If project already in mapping, mark as existing and return
			if (itemMaps.has(absolutePath)) {
				itemMaps.get(absolutePath)!.fileexist = true;
				return;
			}

			// Create FpcTaskProject as IProjectIntf
			const projectIntf = new FpcTaskProject(
				displayName,
				absolutePath,
				false, // Initially not set as default
				null   // No task definition
			);

			// Create FpcItem and add to mapping
			const item = new FpcItem(
				0,
				displayName,
				vscode.TreeItemCollapsibleState.Expanded,
				absolutePath,
				true,
				false,
				ProjectType.FPC,
				projectIntf
			);
			
			// Use absolute path as key to ensure uniqueness
			itemMaps.set(absolutePath, item);
		} catch (error) {
			console.error(`Error collecting FPC project ${file}:`, error);
			vscode.window.showErrorMessage("FPCToolkit:" + Error(<string>error).message);
		}
	}

	/**
	 * Collect Lazarus project
	 * @param file Absolute path or file name
	 * @param itemMaps Project mapping
	 * @param workspaceFolder Workspace folder
	 * @param relativePath Optional relative path for display
	 */
	private collectLazarusProject(file: string, itemMaps: Map<string, FpcItem>, workspaceFolder: vscode.WorkspaceFolder, relativePath?: string): void {
		try {
			const absolutePath = path.isAbsolute(file) ? file : path.join(workspaceFolder.uri.fsPath, file);
			const displayName = relativePath || file;
			
			const projectIntf = this.getProjectInfos(absolutePath);

			if (projectIntf) {
				// Check if project is marked as default
				const hasDefaultTask = projectIntf.tasks && projectIntf.tasks.some(task => task.isDefault);

				// Create project root node
				const rootItem = new FpcItem(
					0,
					displayName,
					vscode.TreeItemCollapsibleState.Expanded,
					absolutePath,
					true,
					hasDefaultTask || false, // Root node default state based on whether there's a default task
					ProjectType.Lazarus,
					projectIntf
				);

				// Add project to mapping using absolute path as key
				itemMaps.set(absolutePath, rootItem);
			}
		} catch (error) {
			console.error(`Error collecting Lazarus project ${file}:`, error);
			vscode.window.showErrorMessage("FPCToolkit:" + Error(<string>error).message);
		}
	}

	/**
	 * Apply default project logic
	 * @param itemMaps Project mapping
	 */
	private applyDefaultProjectLogic(itemMaps: Map<string, FpcItem>): void {
		if (itemMaps.size < 1) {
			return;
		}

		let defaultTask: IProjectTask | undefined;

		// 1. Find default task in FPC projects
		for (const item of itemMaps.values()) {
			if (item.project?.tasks) {
				for (const task of item.project.tasks) {
					if (task.isDefault) {
						defaultTask = task;
						break;
					}
				}
				if (defaultTask) break;
			}
		}

		// 2. If still not found, use the first task as default
		if (!defaultTask) {
			for (const item of itemMaps.values()) {
				if (item.project?.tasks && item.project.tasks.length > 0) {
					defaultTask = item.project.tasks[0];
					break;
				}
			}
		}

		// Apply default status
		if (defaultTask) {
			// Clear default status for all projects
			for (const item of itemMaps.values()) {
				if (item.project?.tasks) {
					for (const task of item.project.tasks) {
						task.isDefault = false;
					}
				}
			}

			// Set default project and task
			defaultTask.isDefault = true;
			
		}
	}

	dispose() {
		this.watch?.dispose();
		this.watchlpr?.dispose();
		this.watchlpi?.dispose();
		this.watchSource?.dispose();
	}


	/*TreeDataProvider*/
	refresh(): void {
		this._onDidChangeTreeData.fire();
	}

	async checkDefaultAndRefresh(): Promise<void> {
		let oldCompileOption = this.defaultCompileOption;
		if (oldCompileOption == undefined) {
			taskProvider.refresh();
			this.refresh();
			return;
		}

		//default task setting changed 
		let newCompileOption = await this.GetDefaultTaskOption();
		if (oldCompileOption.toOptionString() != newCompileOption.toOptionString()) {
			taskProvider.refresh();
		}
		this.refresh();
	}

	getTreeItem(element: FpcItem): vscode.TreeItem {
		return element;
	}

	async getChildren(element?: FpcItem | undefined): Promise<FpcItem[]> {

		if (element) {
		// Handle child nodes (project build configurations)
			let items: FpcItem[] = [];

			if (element.project && element.project.tasks) {
				// Directly use already stored tasks
				for (const task of element.project.tasks) {
					// Create tree item
					let item = new FpcItem(
						1,
						task.label,
						vscode.TreeItemCollapsibleState.None,
						element.file,
						element.fileexist,
						task.isDefault,
						element.projectType,
						task
					);
					items.push(item);

					// If default task, update global default project
					if (item.isDefault) {
						this.defaultFpcItem = item;
					}
				}
			}

			return items;

		} else {
		// Handle root node

		// Create a mapping to store all projects
			var itemMaps: Map<string, FpcItem> = new Map();

		// 1. Collect all project info

		// 1.1 Collect FPC projects from tasks.json
			this.config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));

		// Handle FPC tasks in tasks.json
			this.collectFpcTaskProjects(itemMaps);

		// 1.2 Collect projects from workspace files
			if (vscode.workspace.workspaceFolders) {
				for (const workspaceFolder of vscode.workspace.workspaceFolders) {
					await this.collectProjectsFromWorkspace(workspaceFolder, itemMaps);
				}
			}

		// Apply default project logic
			this.applyDefaultProjectLogic(itemMaps);

			let items: FpcItem[] = [];

		// Add all projects in mapping to project list
			for (const item of itemMaps.values()) {
				items.push(item);
			}

			return items;
		}
	}

	async GetDefaultTaskOption(): Promise<CompileOption> {
		// Check if we have a default FPC item with project interface
		if (this.defaultFpcItem?.project && this.defaultFpcItem.project.tasks && this.defaultFpcItem.project.tasks.length > 0) {
			// Use the first task's compile options
			const opt = this.defaultFpcItem.project.tasks[0].getCompileOption(this.workspaceRoot);
			this.defaultCompileOption = opt;
			return opt;
		}

		//refresh tasks
		await vscode.tasks.fetchTasks({ type: 'fpc' });

		let cfg = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
		let opt: CompileOption | undefined = undefined;
		let is_first = true;
		if (cfg?.tasks != undefined) {
			for (const e of cfg?.tasks) {
				if (e.type === 'fpc') {
					if (e.group?.isDefault) {
						let def = taskProvider.GetTaskDefinition(e.label);
						// Create a FpcTaskProject to get compile options
						const projectIntf = new FpcTaskProject(e.label, e.file, true, def);
						// Use the first task's compile options
						if (projectIntf.tasks && projectIntf.tasks.length > 0) {
							opt = projectIntf.tasks[0].getCompileOption(this.workspaceRoot);
						} else {
							opt = new CompileOption(def, this.workspaceRoot);
						}
						this.defaultCompileOption = opt;
						return opt;
					}
					if (is_first) {
						is_first = false;
						let def = taskProvider.GetTaskDefinition(e.label);
						// Create a FpcTaskProject to get compile options
						const projectIntf = new FpcTaskProject(e.label, e.file, false, def);
						if (projectIntf.tasks.length > 0) {
							opt = projectIntf.tasks[0].getCompileOption(this.workspaceRoot);
						} else {
							opt = new CompileOption(def, this.workspaceRoot);
						}
					}
				}
			}
		}
		if (!opt) {
			opt = new CompileOption();
		}
		this.defaultCompileOption = opt;
		return opt;
	}
}