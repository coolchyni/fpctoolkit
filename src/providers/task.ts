/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as vscode from 'vscode';
import { CompileOption } from '../languageServer/options';
import { pathToFileURL } from 'url';
import * as ChildProcess from "child_process";
import path = require('path');
import { TerminalEscape, TE_Style } from '../common/escape';
import * as fs from 'fs';
import { threadId } from 'worker_threads';
import { SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION } from 'constants';
import { resolve } from 'path';
import { configuration } from '../common/configuration';
import { client } from '../extension';
import { DiagnosticSeverity } from 'vscode';
import { url } from 'inspector';

export class  BuildOption{
	targetOS?: string;
	targetCPU?: string;
	customOptions?: string[];
	libPath?: string[];
	outputFile?: string;
	unitOutputDir?: string;
	optimizationLevel?: number;
	searchPath?: string[];
	syntaxMode?: string;
	forceRebuild?: boolean=false;
	msgIgnore?:Number[];
};


export class FpcTaskDefinition implements vscode.TaskDefinition {
	[name: string]: any;
	readonly type: string='fpc';
	file?: string;
	cwd?: string;
	cleanExt?:string;
	inherited?:string;
	buildOption?:BuildOption;
}


export class FpcTaskProvider implements vscode.TaskProvider {
	static FpcTaskType = 'fpc';
	private defineMap: Map<string,FpcTaskDefinition>=new Map<string,FpcTaskDefinition>();
	public taskMap: Map<string,vscode.Task>=new Map<string,vscode.Task>();
	public GetTaskDefinition(name:string):FpcTaskDefinition|undefined{
		return this.defineMap.get(name);
	}
	constructor(private workspaceRoot: string,private cwd: string|undefined=undefined) { 
	}

	public async clean(){
		this.defineMap.clear();
		this.taskMap.clear();
	}
	public async provideTasks(): Promise<vscode.Task[]> {
		return this.getTasks();
	}

	public resolveTask(_task: vscode.Task): vscode.Task | undefined {
		if(this.taskMap.has(_task.name)){
			let task= this.taskMap.get(_task.name);
			task!.definition=_task.definition;
			return task;
		}else
		{
			const file: string = _task.definition.file;
			if (file) {
				const definition: FpcTaskDefinition = <any>_task.definition;
				if(_task.definition.cwd){
					this.cwd=this.workspaceRoot+'/'+_task.definition.cwd;
				}
				let task=this.getTask(_task.name, definition.file, definition);
				this.taskMap.set(_task.name,task);
				return task;
			}
		}
		
		return undefined;
	}

	private async getTasks(): Promise<vscode.Task[]> {
		return [];
	}
	private mergeDefinition(from:FpcTaskDefinition,to:FpcTaskDefinition){
		to.file=to.file??from.file;
		to.cwd=to.cwd??from.cwd;
		to.cleanExt=to.cleanExt??from.cleanExt;
		if(from.buildOption!=undefined){
			if(to.buildOption===undefined){
				to.buildOption=Object.assign({},from.buildOption);
			}
			else{
				to.buildOption.customOptions=([] as string[]).concat(from.buildOption.customOptions??[],to.buildOption.customOptions??[]);	
				to.buildOption.libPath=([] as string[]).concat(from.buildOption.libPath??[],to.buildOption.libPath??[]);
				to.buildOption.searchPath=([] as string[]).concat(from.buildOption.searchPath??[],to.buildOption.searchPath??[]);
				to.buildOption.msgIgnore=([] as Number[]).concat(from.buildOption.msgIgnore??[],to.buildOption.msgIgnore??[]);
				
				to.buildOption.optimizationLevel=to.buildOption.optimizationLevel??from.buildOption.optimizationLevel;
				to.buildOption.outputFile=to.buildOption.outputFile??from.buildOption.outputFile;
				to.buildOption.syntaxMode=to.buildOption.syntaxMode??from.buildOption.syntaxMode;
				to.buildOption.targetCPU=to.buildOption.targetCPU??from.buildOption.targetCPU;
				to.buildOption.targetOS=to.buildOption.targetOS??from.buildOption.targetOS;
				to.buildOption.unitOutputDir=to.buildOption.unitOutputDir??from.buildOption.unitOutputDir;
				to.buildOption.forceRebuild=to.buildOption.forceRebuild??from.buildOption.forceRebuild;
				
		 	}
		}
		
	}
	public getTask(name: string, file?: string,  definition?: FpcTaskDefinition): vscode.Task {
		if(definition?.inherited){
			let pdefine=this.defineMap.get(definition.inherited);
			if(pdefine){
				let realDefinition=new FpcTaskDefinition();
				this.mergeDefinition(definition,realDefinition);
				this.mergeDefinition(pdefine,realDefinition);
				this.defineMap.set(name,realDefinition);
				let task = new FpcTask(this.cwd?this.cwd:this.workspaceRoot, name, file!, definition,realDefinition);
				return task;
			}

		}
		this.defineMap.set(name,definition!);
		let task = new FpcTask(this.cwd?this.cwd:this.workspaceRoot, name, file!, definition!);


		// task.presentationOptions.clear = true;
		// task.presentationOptions.echo = true;
		// task.presentationOptions.focus = false;
		// task.presentationOptions.showReuseMessage = false;
		// task.presentationOptions.reveal = vscode.TaskRevealKind.Always;
		// task.presentationOptions.panel = vscode.TaskPanelKind.Shared;
		//  task.presentationOptions.revealProblems='onProblem';
		//(task.presentationOptions as any)["revealProblems"]="onProblem";

		//task.problemMatchers.push('$fpc');
	
		
		return task;
	}

	public refresh(){
		client.restart();
	}
}

export enum BuildMode{
	normal,
	rebuild
}
export class FpcTask extends vscode.Task  {
	private _TaskBuildOptionString: string = '';
	private _BuildMode: BuildMode = BuildMode.normal;
	public get BuildMode(): BuildMode {
		return this._BuildMode;
	}
	public set BuildMode(value: BuildMode) {
		this._BuildMode = value;
	}
	public get TaskBuildOptionString(): string {
		return this._TaskBuildOptionString;
	}
	public set TaskBuildOptionString(value: string) {
		this._TaskBuildOptionString = value;
	}
	constructor(cwd: string, name: string, file: string, taskDefinition: FpcTaskDefinition,realDefinition?: FpcTaskDefinition) {

		let buildOptionString: string='';
		if(realDefinition===undefined){
			realDefinition=taskDefinition;
		}
		if (realDefinition?.buildOption) {
			let opt: CompileOption = new CompileOption(realDefinition);
			buildOptionString = opt.toOptionString();
		}
		if (!buildOptionString) {
			buildOptionString = "";
		}

		if (!realDefinition) {
			realDefinition = {
				type: FpcTaskProvider.FpcTaskType,
				file: file,

			};

		}
		buildOptionString+='-vq '; //show message numbers 

		let fpcpath =process.env['PP'];//  configuration.get<string>('env.PP');
		if(fpcpath===''){
			fpcpath='fpc';
		}

		
		super(
			taskDefinition,
			vscode.TaskScope.Workspace,
			`${name}`,
			FpcTaskProvider.FpcTaskType,
			//new vscode.ShellExecution(`${fpcpath} ${taskDefinition.file} ${buildOptionString}`)
			new FpcCustomExecution(async (): Promise<vscode.Pseudoterminal> => {
				// 	// When the task is executed, this callback will run. Here, we setup for running the task.
				// let terminal = new  FpcBuildTaskTerminal(workspaceRoot, fpcpath!);
				//terminal.args =  `${taskDefinition?.file} ${buildOptionString}`.split(' ');
				let terminal = new  FpcBuildTaskTerminal(cwd, fpcpath!);
				
				terminal.args = `${taskDefinition?.file} ${buildOptionString}`.split(' ');
				if(this._BuildMode==BuildMode.rebuild){
					terminal.args.push('-B');	
				}
				return  terminal;
				
			}) 
		);
		this.TaskBuildOptionString=buildOptionString;
	}


}

class FpcCustomExecution extends vscode.CustomExecution {

}
export var diagCollection: vscode.DiagnosticCollection = vscode.languages.createDiagnosticCollection('fpc');

class FpcBuildTaskTerminal implements vscode.Pseudoterminal,vscode.TerminalExitStatus {
	private writeEmitter = new vscode.EventEmitter<string>();
	onDidWrite: vscode.Event<string> = this.writeEmitter.event;
	private closeEmitter = new vscode.EventEmitter<number>();
	onDidClose: vscode.Event<number> = this.closeEmitter.event;

	private process?: ChildProcess.ChildProcess;
	protected buffer: string = "";
	protected errbuf: string = "";

	private diagMaps: Map<string, vscode.Diagnostic[]>;
	public args: string[] = [];

	constructor(private cwd: string, private fpcpath: string) {
		this.diagMaps = new Map<string, vscode.Diagnostic[]>();
		this.onDidClose((e) => {
			//vscode.window.showInformationMessage('onDidClose');	
		});
		
	}
	code: number | undefined;

	// private static inst?: FpcBuildTaskTerminal;
	// static getInstance(workspaceRoot?: string, fpcpath?: string): FpcBuildTaskTerminal {
	// 	if (FpcBuildTaskTerminal.inst) {
	// 		return FpcBuildTaskTerminal.inst;
	// 	} else {
	// 		FpcBuildTaskTerminal.inst = new FpcBuildTaskTerminal(workspaceRoot!, fpcpath!);
	// 		return FpcBuildTaskTerminal.inst;
	// 	}

	// }
	clear() {
	
	}
	open(initialDimensions: vscode.TerminalDimensions | undefined): void {
		//vscode.window.createTerminal()
		// At this point we can start using the terminal.
		this.doBuild();
	}

	close(): void {
	
	}
	

	async buildend() {
		let units=Array.from(this.diagMaps.keys());
	
		// The terminal has been closed. Shutdown the build.
		diagCollection.clear();
		let has_error: boolean = false;
		for (const iter of this.diagMaps) {
			let key=iter[0];
			let item=iter[1];
			let uri:vscode.Uri|undefined=undefined;
			if(fs.existsSync(key)){
				uri=vscode.Uri.file(key);
			}
			if(!uri)
			{
				let unit=key.split(".")[0];

				let unitpaths=await client.getUnitPath([unit]);
				if(unitpaths.length<1){
					return;
				}
				let unitpath=unitpaths[0];
				//let uri:vscode.Uri|undefined=vscode.Uri.file(unitpath);
				if(unitpath==''){
					uri=this.findFile(key)!;
				}else{
					uri=vscode.Uri.file(unitpath);
				}
			}
			
			if(uri){
				diagCollection.set(uri,item);
			}else
			{
				diagCollection.set(vscode.Uri.file(key),item);
			}
			if (!has_error) {
			
				item.forEach((d) => {
					if (d.severity === 0) {
						has_error = true;
					}
				});
			}
		}

		if (has_error) {
			vscode.commands.executeCommand('workbench.actions.view.problems');
		}
	}
	findFile(filename: string): vscode.Uri | undefined {

		let f = path.join(this.cwd, filename);
		if (fs.existsSync(f)) {
			return vscode.Uri.file(f);
		}
		for (let index = 0; index < this.args.length; index++) {
			const e = this.args[index];
			if (e.startsWith('-Fu')) {
				let f2 = e.substring(3);
				if (f2.startsWith('.')) {
					f = path.join(this.cwd, f2, filename);
				} else {
					f = path.join(f2, filename);
				}
				if (fs.existsSync(f)) {
					return vscode.Uri.file(f);
				}
			}
		}
		return undefined;
	}


	private async doBuild(): Promise<number> {
		return new Promise<number>((resolve) => {
			
			this.buffer = "";
			this.errbuf = "";
			this.diagMaps.clear();
			this.emit(TerminalEscape.apply({ msg: `${this.fpcpath} ${this.args.join(' ')}\r\n`, style: [TE_Style.Bold] }));
			this.process = ChildProcess.spawn(this.fpcpath, this.args, { cwd: this.cwd });

			this.process.stdout?.on('data', this.stdout.bind(this));
			this.process.stderr?.on('data', this.stderr.bind(this));
			this.process.on('close', (code) => {
				
				this.writeEmitter.fire(`Exited with code ${code}.\r\nBuild complete. \r\n\r\n`);
				this.buildend().then(()=>{
					this.closeEmitter.fire(code);
				});
				//This is a exitcode,not zero meens failure.
				
				
				resolve(0);
			});

		});
	}


	emit(msg: string) {
		this.writeEmitter.fire(msg + '\r\n');
	}
	stdout(data: any) {
		if (typeof data === "string") {
			this.buffer += data;
		}
		else {
			this.buffer += data.toString("utf8");
		}
		const end = this.buffer.lastIndexOf('\n');
		if (end !== -1) {
			this.onOutput(this.buffer.substr(0, end));
			this.buffer = this.buffer.substr(end + 1);
		}
		// if (this.buffer.length) {
		// 	this.emit(this.buffer);
		// }
	}

	stderr(data: any) {
		if (typeof data === "string") {
			this.emit(TerminalEscape.apply({ msg: data, style: [TE_Style.Yellow] }));

		}
		else {
			this.emit(TerminalEscape.apply({ msg: data.toString("utf8"), style: [TE_Style.Yellow] }));
		}
	}
	getDiagnosticSeverity(level: string) {
		switch (level) {
			case 'Fatal':
			case 'Error':
				return vscode.DiagnosticSeverity.Error;
			case 'Warning':
				return vscode.DiagnosticSeverity.Warning;
			case 'Note':
				return vscode.DiagnosticSeverity.Information;
			case 'Hint':
				return vscode.DiagnosticSeverity.Hint;
			default:
				return vscode.DiagnosticSeverity.Information;
		}
	}
	onOutput(lines: string) {
		let ls = <string[]>lines.split('\n');
		let cur_file="";
		let reg = /^(([-:\w\\\/]+)\.(p|pp|pas|lpr|dpr|inc))\(((\d+)(\,(\d+))?)\)\s(Fatal|Error|Warning|Note|Hint): \((\d+)\) (.*)/	
		ls.forEach(line => {
		
			let matchs = reg.exec(line);

			if (matchs) {
				
				let ln = Number(matchs[5]);
				let col = Number(matchs[7]);
				let file = matchs[1];
				let unit= matchs[2];
				let level = matchs[8];
				let msgcode= matchs[9];
				let msg = matchs[10];
				// this.emit(
				// 	TerminalEscape.apply({ msg: file+"("+ln+','+col +") ", style: TE_Style.Blue })+
				// 	TerminalEscape.apply({ msg: level+":"+msg, style: TE_Style.Red })
				// );

				let diag = new vscode.Diagnostic(
					new vscode.Range(new vscode.Position(ln - 1, col - 1), new vscode.Position(ln - 1, col - 1)),
					msg,
					this.getDiagnosticSeverity(level)
				);
				diag.code=Number.parseInt(msgcode);

				// if(msg.match(/ Local variable ".*?".*?(?:not|never) used/))
				// {
				// 	diag.code='variable-not-used';
				// }
				let basename=path.basename(file);
				// if((cur_file=="")||(path.basename(cur_file)!=path.basename(file))){
				// 	cur_file=file;
				// }
				if (this.diagMaps?.has(basename)) {

					this.diagMaps.get(basename)?.push(diag);
				} else {

					this.diagMaps.set(basename, [diag]);

				}
				if(diag.severity==DiagnosticSeverity.Error){
					this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
				}else
				{
					this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));
				}

			} else if (line.startsWith('Error:') || line.startsWith('Fatal:')) { //Fatal|Error|Warning|Note
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));

			} else if (line.startsWith('Warning:')) {
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.BrightYellow] }));
			}
			else {
				this.emit(line);
			}
		});
	}

}

export let  taskProvider:FpcTaskProvider;

if (vscode.workspace.workspaceFolders) {
	const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;
	taskProvider=new FpcTaskProvider(workspaceRoot);	
}

