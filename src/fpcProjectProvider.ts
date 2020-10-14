import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { basename } from 'path';
import { CompileOption, TaskInfo } from './fpcSetting';
import { openStdin } from 'process';

export class FpcProjectProvider implements vscode.TreeDataProvider<FpcItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<FpcItem | undefined | void> = new vscode.EventEmitter<FpcItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<FpcItem | undefined | void> = this._onDidChangeTreeData.event;
	private watch!: vscode.FileSystemWatcher;
	private watchlpr!: vscode.FileSystemWatcher;
	constructor(private workspaceRoot: string) {
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
				items.push(new FpcItem(
					1,
					task.label,
					vscode.TreeItemCollapsibleState.None,
					element.file,
					element.fileexist,
					[task]
				));
			});
			// element.fprSetting?.compileOptions.forEach((option) => {

			// 	items.push(new FpcItem(
			// 		1,
			// 		option.label,
			// 		vscode.TreeItemCollapsibleState.None,
			// 		element.file,
			// 		element.fileexist
			// 		//element.fprSetting,
			// 		//option
			// 	));
			// });
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
									true

								)
							);

						} catch (error) {
							vscode.window.showErrorMessage("FPCToolkit:" + Error(error).message);
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
}

export class FpcItem extends vscode.TreeItem {


	constructor(
		public readonly level: number,
		public readonly label: string,
		public readonly collapsibleState: vscode.TreeItemCollapsibleState,
		public readonly file: string,
		public fileexist: boolean,
		public tasks?: any[]
	) {
		super(label, collapsibleState);
		if (level === 0) {
			this.contextValue = 'fpcproject';
		} else {
			this.contextValue = 'fpcbuild';
		}

	}

	get tooltip(): string {
		return `${basename(this.label)}`;
	}

	get description(): string {
		return '';
	}

	iconPath = {
		light: path.join(__filename, '..', '..', 'images', this.level ? 'build.png' : 'pascal-project.png'),
		dark: path.join(__filename, '..', '..', 'images', this.label ? 'build.png' : 'pascal-project.png')
	};


}