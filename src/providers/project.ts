import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { basename, normalize } from 'path';
import { CompileOption, TaskInfo } from '../languageServer/options';
import { openStdin } from 'process';
import { FpcTaskDefinition, FpcTaskProvider, taskProvider } from './task';
import { Command } from 'vscode-languageserver-types';
//import { visit, JSONVisitor } from "jsonc-parser";
import { pathExists } from 'fs-extra';
import { Event } from 'vscode-languageclient';
import { clearTimeout } from 'timers';
import { TIMEOUT } from 'dns';
import { LazarusProjectParser, LazarusProjectInfo } from './lazarus';

export enum ProjectType {
	FPC = 'fpc',
	Lazarus = 'lazarus'
}

export class FpcProjectProvider implements vscode.TreeDataProvider<FpcItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<FpcItem | undefined | void> = new vscode.EventEmitter<FpcItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<FpcItem | undefined | void> = this._onDidChangeTreeData.event;
	private watch!: vscode.FileSystemWatcher;
	private watchlpr!: vscode.FileSystemWatcher;
	private watchlpi!: vscode.FileSystemWatcher; // 监控Lazarus项目文件
	private watchSource!: vscode.FileSystemWatcher; // 监控源文件变化
	public defaultFpcItem?: FpcItem = undefined;
	private config!: vscode.WorkspaceConfiguration;
	private defaultCompileOption?: CompileOption = undefined;
	private timeout?: NodeJS.Timeout = undefined;
	private _hasSourceFileChanged: boolean = false; // 标志源文件是否有变化
	constructor(private workspaceRoot: string, context: vscode.ExtensionContext) {
		const subscriptions = context.subscriptions;
		const name = 'FpcProjectExplorer';
		subscriptions.push(vscode.commands.registerCommand(name + ".open", async (item: FpcItem) => { await this.open(item); }, this));

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

		// 监控Lazarus项目文件
		this.watchlpi = vscode.workspace.createFileSystemWatcher("**/*.lpi", false, true, false);
		this.watchlpi.onDidCreate(() => {
			this.refresh();
		});
		this.watchlpi.onDidDelete(() => {
			this.refresh();
		});

		// 监控所有 Pascal 源文件的变化
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
	 * 检查源文件是否有变化
	 */
	public hasSourceFileChanged(): boolean {
		return this._hasSourceFileChanged;
	}

	/**
	 * 重置源文件变化标志
	 */
	public resetSourceFileChanged(): void {
		this._hasSourceFileChanged = false;
	}

	/**
	 * 确保获取到默认的FPC项目
	 * 如果当前没有默认项目，会主动计算并设置
	 */
	public async ensureDefaultFpcItem(): Promise<FpcItem | undefined> {
		if (this.defaultFpcItem) {
			return this.defaultFpcItem;
		}

		// 主动计算默认项目
		await this.computeDefaultFpcItem();
		return this.defaultFpcItem;
	}

	/**
	 * 计算默认的FPC项目
	 */
	private async computeDefaultFpcItem(): Promise<void> {
		this.config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));

		// 首先检查tasks.json中的FPC任务
		if (this.config?.tasks) {
			// 首先查找明确标记为默认的任务
			for (const task of this.config.tasks) {
				if (task.type === 'fpc' && task.group?.isDefault) {
					this.defaultFpcItem = new FpcItem(
						1,
						task.label,
						vscode.TreeItemCollapsibleState.None,
						task.file,
						true,
						true,
						[task],
						ProjectType.FPC
					);
					this.defaultFpcItem.description = 'default';
					return;
				}
			}

			// 如果没有明确的默认任务，使用第一个FPC任务
			for (const task of this.config.tasks) {
				if (task.type === 'fpc') {
					this.defaultFpcItem = new FpcItem(
						1,
						task.label,
						vscode.TreeItemCollapsibleState.None,
						task.file,
						true,
						false,
						[task],
						ProjectType.FPC
					);
					this.defaultFpcItem.description = 'default';
					this.defaultFpcItem.isDefault = true;
					return;
				}
			}
		}

		// 如果没有找到FPC任务，查找Lazarus项目文件作为默认项目
		if (!this.defaultFpcItem && vscode.workspace.workspaceFolders) {
			for (const workspaceFolder of vscode.workspace.workspaceFolders) {
				const files = fs.readdirSync(workspaceFolder.uri.fsPath);

				// 查找第一个.lpi文件作为默认项目
				for (const file of files) {
					if (file.toLowerCase().endsWith('.lpi')) {
						try {
							const lpiPath = path.join(workspaceFolder.uri.fsPath, file);
							const projectInfo = LazarusProjectParser.parseLpiFile(lpiPath);

							if (projectInfo) {
								const taskDef = LazarusProjectParser.createTaskDefinitionFromLpi(projectInfo, lpiPath);

								this.defaultFpcItem = new FpcItem(
									1,
									file,
									vscode.TreeItemCollapsibleState.None,
									file,
									true,
									true,
									[taskDef],
									ProjectType.Lazarus
								);
								this.defaultFpcItem.description = 'default';
								return;
							}
						} catch (error) {
							console.error(`Error processing Lazarus project ${file}:`, error);
						}
					}
				}
			}
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

	getChildren(element?: FpcItem | undefined): vscode.ProviderResult<FpcItem[]> {

		if (element) {
			// 处理子项时，临时保存当前的默认项目，避免丢失
			let tempDefaultItem = this.defaultFpcItem;
			let items: FpcItem[] = [];

			element.tasks?.forEach((task) => {
				let item = new FpcItem(
					1,
					task.label,
					vscode.TreeItemCollapsibleState.None,
					element.file,
					element.fileexist,
					task.group?.isDefault,
					[task],
					element.projectType
				);
				items.push(item);
				if (item.isDefault) {
					this.defaultFpcItem = item;
				}
			});

			// 如果在当前元素的任务中没有找到默认项目，但有任务存在
			if (!this.defaultFpcItem && items.length > 0) {
				this.defaultFpcItem = items[0];
				this.defaultFpcItem.description = 'default';
				this.defaultFpcItem.isDefault = true;
			}

			// 如果仍然没有默认项目，恢复之前的默认项目
			if (!this.defaultFpcItem && tempDefaultItem) {
				this.defaultFpcItem = tempDefaultItem;
			}

			return Promise.resolve(items);

		} else {
			//root node 

			var itemMaps: Map<string, FpcItem> = new Map();
			this.config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
			//create info for pass tasks as pointer
			//var info =new TaskInfo();
			//info.tasks=config.tasks;


			this.config?.tasks?.forEach((e: any) => {
				if (e.type === 'fpc') {
					if (!itemMaps.has(e.file)) {
						itemMaps.set(
							e.file,
							new FpcItem(
								0,
								path.basename(e.file),
								vscode.TreeItemCollapsibleState.Expanded,
								e.file,
								true,
								e.group?.isDefault,
								[e],
								ProjectType.FPC
							)
						);
					} else {
						itemMaps.get(e.file)?.tasks?.push(e);
					}
				}

			});
			let items: FpcItem[] = [];


			vscode.workspace.workspaceFolders!.forEach(item => {
				let files = fs.readdirSync(item.uri.fsPath);
				for (let index = 0; index < files.length; index++) {

					let file = files[index];

					if (file.toLowerCase().endsWith('.lpr') || file.toLowerCase().endsWith('.dpr')) {
						try {
							if (itemMaps.has(file)) {
								itemMaps.get(file)!.fileexist = true;
								continue;
							}

							itemMaps.set(
								file,
								new FpcItem(
									0,
									file,
									vscode.TreeItemCollapsibleState.Expanded,
									file,
									true,
									false,
									undefined,
									ProjectType.FPC
								)
							);

						} catch (error) {
							vscode.window.showErrorMessage("FPCToolkit:" + Error(<string>error).message);
						}
					} else if (file.toLowerCase().endsWith('.lpi')) {
						// 处理Lazarus项目文件
						try {
							const lpiPath = path.join(item.uri.fsPath, file);
							const projectInfo = LazarusProjectParser.parseLpiFile(lpiPath);

							if (projectInfo) {
								const taskDef = LazarusProjectParser.createTaskDefinitionFromLpi(projectInfo, lpiPath);

								itemMaps.set(
									file,
									new FpcItem(
										0,
										file,
										vscode.TreeItemCollapsibleState.Expanded,
										file,
										true,
										false,
										[taskDef],
										ProjectType.Lazarus
									)
								);
							}
						} catch (error) {
							vscode.window.showErrorMessage("FPCToolkit:" + Error(<string>error).message);
						}
					}
				}
			});

			for (const e of itemMaps.values()) {
				items.push(e);
			}

			// 确保设置默认项目（在根节点处理完毕后）
			this.computeDefaultFpcItem();

			return Promise.resolve(items);
		}

		return Promise.resolve([]);

	}
	async GetDefaultTaskOption(): Promise<CompileOption> {

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

						opt = new CompileOption(def, this.workspaceRoot);
						this.defaultCompileOption = opt;
						return opt;
					}
					if (is_first) {
						is_first = false;
						let def = taskProvider.GetTaskDefinition(e.label);
						opt = new CompileOption(def, this.workspaceRoot);
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
	private findJsonDocumentPosition(documentText: string, taskItem: FpcItem) {
		// const me = this;
		// let inScripts = false;
		// let inTasks = false;
		// let inTaskLabel: any;
		// let scriptOffset = 0;


		// const visitor: JSONVisitor =
		// {
		// 	onError: () => {
		// 		return scriptOffset;
		// 	},
		// 	onObjectEnd: () => {
		// 		if (inScripts) {
		// 			inScripts = false;
		// 		}
		// 	},
		// 	onLiteralValue: (value: any, offset: number, _length: number) => {
		// 		if (inTaskLabel) {
		// 			if (typeof value === "string") {
		// 				if (inTaskLabel === "label" || inTaskLabel === "script") {

		// 					if (taskItem.label === value) {
		// 						scriptOffset = offset;
		// 					}
		// 				}
		// 			}
		// 			inTaskLabel = undefined;
		// 		}
		// 	},
		// 	onObjectProperty: (property: string, offset: number, _length: number) => {
		// 		if (property === "tasks") {
		// 			inTasks = true;
		// 			if (!inTaskLabel) { // select the script section
		// 				scriptOffset = offset;
		// 			}
		// 		}
		// 		else if ((property === "label" || property === "script") && inTasks && !inTaskLabel) {
		// 			inTaskLabel = "label";
		// 			if (!inTaskLabel) { // select the script section
		// 				scriptOffset = offset;
		// 			}
		// 		}
		// 		else { // nested object which is invalid, ignore the script
		// 			inTaskLabel = undefined;
		// 		}
		// 	}
		// };

		// visit(documentText, visitor);

		// //log.methodDone("find json document position", 3, "   ", false, [["position", scriptOffset]]);
		// return scriptOffset;
		return documentText.indexOf('"label": "' + taskItem.label + '"');
	}
	private async open(selection: FpcItem) {
		if (selection.projectType === ProjectType.Lazarus) {
			// 对于Lazarus项目，打开.lpi文件
			const lpiFile = vscode.Uri.file(path.join(this.workspaceRoot, selection.file));
			if (fs.existsSync(lpiFile.fsPath)) {
				const document: vscode.TextDocument = await vscode.workspace.openTextDocument(lpiFile);
				await vscode.window.showTextDocument(document);
			}
		} else {
			// 对于FPC项目，打开tasks.json
			let taskfile = vscode.Uri.file(path.join(this.workspaceRoot, '.vscode', 'tasks.json'))

			if (fs.existsSync(taskfile.fsPath)) {
				const document: vscode.TextDocument = await vscode.workspace.openTextDocument(taskfile);
				const offset = this.findJsonDocumentPosition(document.getText(), selection);
				const position = document.positionAt(offset);
				await vscode.window.showTextDocument(document, { selection: new vscode.Selection(position, position) });
			}
		}
	}

}

export class FpcItem extends vscode.TreeItem {


	constructor(
		public readonly level: number,
		public readonly label: string,
		public readonly collapsibleState: vscode.TreeItemCollapsibleState,
		public readonly file: string,
		public fileexist: boolean,
		public isDefault: boolean,
		public tasks?: any[],
		public projectType: ProjectType = ProjectType.FPC
	) {
		super(label, collapsibleState);
		if (level === 0) {
			this.contextValue = projectType === ProjectType.Lazarus ? 'lazarusproject' : 'fpcproject';
		} else {
			this.contextValue = 'fpcbuild';
		}
		this.tooltip = `${basename(this.label)} `;
		if (this.level > 0) {
			this.description = this.isDefault ? 'default' : '';

			const command = {
				command: "FpcProjectExplorer.open", // commandId is a string that contains the registered id ('myExtension.debugMessage')
				title: '',
				arguments: [this]
			};
			this.command = command;
		}

		// 根据项目类型设置不同的图标
		if (this.level === 0) {
			if (projectType === ProjectType.Lazarus) {
				this.iconPath = new vscode.ThemeIcon('package');
			} else {
				this.iconPath = path.join(__filename, '..', '..', 'images', 'pascal-project.png');
			}
		} else {
			this.iconPath = new vscode.ThemeIcon('wrench');
		}

		//https://code.visualstudio.com/api/references/icons-in-labels

		//this.command!.command= "workbench.action.tasks.configureTaskRunner"; 
		//this.command!.arguments?.push(this.id);

	}


	// iconPath = {
	// 	light: this.level?'$(gripper)':path.join(__filename, '..','..',  'images', this.level ? 'build.png' : 'pascal-project.png'),
	// 	dark: path.join(__filename, '..','..',  'images', this.label ? 'build.png' : 'pascal-project.png')
	// };


}