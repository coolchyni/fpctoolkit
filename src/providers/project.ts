import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { basename, normalize } from 'path';
import { CompileOption, TaskInfo } from '../languageServer/options';
import { openStdin } from 'process';
import { FpcTaskDefinition } from './task';
import { Command } from 'vscode-languageserver-types';
import { visit, JSONVisitor } from "jsonc-parser";
import { pathExists } from 'fs-extra';

export class FpcProjectProvider implements vscode.TreeDataProvider<FpcItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<FpcItem | undefined | void> = new vscode.EventEmitter<FpcItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<FpcItem | undefined | void> = this._onDidChangeTreeData.event;
	private watch!: vscode.FileSystemWatcher;
	private watchlpr!: vscode.FileSystemWatcher;
	public defaultFtpItem?: FpcItem = undefined;
	constructor(private workspaceRoot: string, context: vscode.ExtensionContext) {
		const subscriptions = context.subscriptions;
		const name = 'FpcProjectExplorer';
		subscriptions.push(vscode.commands.registerCommand(name + ".open", async (item: FpcItem) => { await this.open(item); }, this));

		this.watch = vscode.workspace.createFileSystemWatcher("**/tasks.json", true);
		this.watch.onDidChange((url) => {
			this.refresh();
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

	}


	dispose() {
		throw new Error("Method not implemented.");
	}


	/*TreeDataProvider*/
	refresh(): void {
		this._onDidChangeTreeData.fire();
	}

	getTreeItem(element: FpcItem): vscode.TreeItem {
		return element;
	}

	getChildren(element?: FpcItem | undefined): vscode.ProviderResult<FpcItem[]> {


		if (element) {
			let items: FpcItem[] = [];
			element.tasks?.forEach((task) => {
				let item = new FpcItem(
					1,
					task.label,
					vscode.TreeItemCollapsibleState.None,
					element.file,
					element.fileexist,
					task.group?.isDefault,
					[task]
				);
				items.push(item);
				if (item.isDefault) {
					this.defaultFtpItem = item;
				}
			});
			return Promise.resolve(items);

		} else {
			//root node 

			var itemMaps: Map<string, FpcItem> = new Map();
			let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
			//create info for pass tasks as pointer
			//var info =new TaskInfo();
			//info.tasks=config.tasks;


			config?.tasks?.forEach((e: any) => {
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
								[e]
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
									false

								)
							);

						} catch (error) {
							vscode.window.showErrorMessage("FPCToolkit:" + Error(<string>error).message);
						}


					}
				}
			});

			for (const e of itemMaps.values()) {
				items.push(e);
			}


			//});  
			// if(info.ischanged){
			// 	config.update("tasks",info.tasks,vscode.ConfigurationTarget.WorkspaceFolder);
			// }

			return Promise.resolve(items);
		}

		return Promise.resolve([]);

	}

	GetDefaultTaskOption(): CompileOption | undefined {
		let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
		let opt: CompileOption|undefined=undefined;
		if (config?.tasks != undefined) {
			for (const e of config?.tasks) {
				if (e.type === 'fpc') {
					opt = new CompileOption(e.file, e.file);
					opt.buildOption = e.buildOption;
					if (e.cwd) {
						opt.cwd = path.join(this.workspaceRoot, e.cwd);
					} else {
						opt.cwd = this.workspaceRoot;
					}
					if (e.group?.isDefault) {

						return opt;
					}
				}
			}
		}
		if(!opt){
			opt=new CompileOption('','');
		}

		return opt;
	}
	private findJsonDocumentPosition(documentText: string, taskItem: FpcItem) {
		const me = this;
		let inScripts = false;
		let inTasks = false;
		let inTaskLabel: any;
		let scriptOffset = 0;


		const visitor: JSONVisitor =
		{
			onError: () => {
				return scriptOffset;
			},
			onObjectEnd: () => {
				if (inScripts) {
					inScripts = false;
				}
			},
			onLiteralValue: (value: any, offset: number, _length: number) => {
				if (inTaskLabel) {
					if (typeof value === "string") {
						if (inTaskLabel === "label" || inTaskLabel === "script") {

							if (taskItem.label === value) {
								scriptOffset = offset;
							}
						}
					}
					inTaskLabel = undefined;
				}
			},
			onObjectProperty: (property: string, offset: number, _length: number) => {
				if (property === "tasks") {
					inTasks = true;
					if (!inTaskLabel) { // select the script section
						scriptOffset = offset;
					}
				}
				else if ((property === "label" || property === "script") && inTasks && !inTaskLabel) {
					inTaskLabel = "label";
					if (!inTaskLabel) { // select the script section
						scriptOffset = offset;
					}
				}
				else { // nested object which is invalid, ignore the script
					inTaskLabel = undefined;
				}
			}
		};

		visit(documentText, visitor);

		//log.methodDone("find json document position", 3, "   ", false, [["position", scriptOffset]]);
		return scriptOffset;
	}
	private async open(selection: FpcItem) {

		let taskfile = vscode.Uri.file(path.join(this.workspaceRoot, '.vscode', 'tasks.json'))

		fs.existsSync(taskfile.fsPath)
		{
			const document: vscode.TextDocument = await vscode.workspace.openTextDocument(taskfile);
			const offset = this.findJsonDocumentPosition(document.getText(), selection);
			const position = document.positionAt(offset);
			await vscode.window.showTextDocument(document, { selection: new vscode.Selection(position, position) });
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
		public tasks?: any[]
	) {
		super(label, collapsibleState);
		if (level === 0) {
			this.contextValue = 'fpcproject';
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

		//this.iconPath="$(play)";
		//https://code.visualstudio.com/api/references/icons-in-labels

		//this.command!.command= "workbench.action.tasks.configureTaskRunner"; 
		//this.command!.arguments?.push(this.id);

	}



	iconPath = {
		light: path.join(__filename, '..', '..', '..', 'images', this.level ? 'build.png' : 'pascal-project.png'),
		dark: path.join(__filename, '..', '..', '..', 'images', this.label ? 'build.png' : 'pascal-project.png')
	};


}