import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { basename } from 'path';
import { FpcSetting, CompileOption, TaskInfo } from './fpcSetting';

export class FpcProjectProvider implements vscode.TreeDataProvider<FpcItem> {

	private _onDidChangeTreeData: vscode.EventEmitter<FpcItem | undefined | void> = new vscode.EventEmitter<FpcItem | undefined | void>();
	readonly onDidChangeTreeData: vscode.Event<FpcItem | undefined | void> = this._onDidChangeTreeData.event;
	private watch!:vscode.FileSystemWatcher;
	private watchlpr!:vscode.FileSystemWatcher;
	constructor(private workspaceRoot: string) {
		this.watch=vscode.workspace.createFileSystemWatcher("**/tasks.json",true);
		this.watch.onDidChange(()=>{
			this.refresh();
		});
		this.watch.onDidDelete(()=>{
			this.refresh();
		});

		this.watchlpr=vscode.workspace.createFileSystemWatcher("**/*.lpr",false,true,false);
		this.watchlpr.onDidCreate(()=>{
			this.refresh();
		});
		this.watchlpr.onDidDelete(()=>{
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

		
		if(element){
			var items: FpcItem[]=[];
			 element.fprSetting?.compileOptions.forEach((option)=>{

				items.push(new FpcItem(
					1,
					option.label,
					vscode.TreeItemCollapsibleState.None,
					element.file,
					element.fprSetting,
					option
			   ));
			 });
			 return Promise.resolve(items);

		}else{
			
				var items: FpcItem[]=[];
				let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
				//定义成这样是为了能够按地址传递 
				var info =new TaskInfo();
				info.tasks=config.tasks;
				
				vscode.workspace.workspaceFolders!.forEach(item => {
					let files=fs.readdirSync(item.uri.fsPath);
					for (let index = 0; index < files.length; index++) {
						
						let file=files[index];

						if(file.toLowerCase().endsWith('.lpr'))
						{
							try {
								let label=file.substring(0,file.length-4);
							
								let fsetting=new FpcSetting(info,file);

								items.push(new FpcItem(
									0,
									label,
									vscode.TreeItemCollapsibleState.Expanded,
									file,
									fsetting
									
								));
							} catch (error) {
								vscode.window.showErrorMessage(Error(error).message);
							}
							
							
						}
					}
					
						
	
					
				});  
				if(info.ischanged){
					config.update("tasks",info.tasks,vscode.ConfigurationTarget.WorkspaceFolder);
				}
				return Promise.resolve(items);
		}

        return Promise.resolve([]);

	}
}

export class FpcItem extends vscode.TreeItem {
 
	
	constructor(
		public readonly level:number,
		public readonly label: string,
		public readonly collapsibleState: vscode.TreeItemCollapsibleState,
		public readonly file: string,
		public fprSetting?:FpcSetting,
		public compileOption?:CompileOption,
		public readonly command?: vscode.Command
	) {
		super(label, collapsibleState);
		if(level===0){
			this.contextValue='fpcproject';
		}else {
			this.contextValue='fpcbuild';
		}
		
	}

	get tooltip(): string {
		return `${basename(this.label)}`;
	}

	get description(): string {
		return '';
	}

	iconPath = {
		light: path.join(__filename,'..','..', 'images', this.level?'build.png':'pascal-project.png'),
		dark: path.join(__filename, '..','..', 'images', this.label?'build.png':'pascal-project.png' )
	};


}