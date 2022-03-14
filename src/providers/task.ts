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
export interface FpcTaskDefinition extends vscode.TaskDefinition {
	/**
	 *
	 */
	file: string,
	/**
	 * The options for build. 
	 */

}


export class FpcTaskProvider implements vscode.TaskProvider {
	static FpcTaskType = 'fpc';


	constructor(private workspaceRoot: string,private cwd: string|undefined=undefined) { 
	}

	public async provideTasks(): Promise<vscode.Task[]> {
		return this.getTasks();
	}

	public resolveTask(_task: vscode.Task): vscode.Task | undefined {
		const file: string = _task.definition.file;
		if (file) {
			const definition: FpcTaskDefinition = <any>_task.definition;
			if(_task.definition.cwd){
				this.cwd=this.workspaceRoot+'/'+_task.definition.cwd;
			}
			
			return this.getTask(_task.name, definition.file, definition.buildOption, definition);
		}
		return undefined;
	}

	private getTasks(): vscode.Task[] {
		return [];
	}

	public getTask(name: string, file: string, buildOptionString?: string, definition?: FpcTaskDefinition): vscode.Task {
		let task = new FpcTask(this.cwd?this.cwd:this.workspaceRoot, name, file, buildOptionString, definition);

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

}

class FpcTask extends vscode.Task  {
	constructor(cwd: string, name: string, file: string, buildOptionString?: string, taskDefinition?: FpcTaskDefinition) {

		if (taskDefinition?.buildOption) {
			let opt: CompileOption = new CompileOption(taskDefinition.file, taskDefinition.file);
			opt.buildOption = taskDefinition.buildOption;
			buildOptionString = opt.toOptionString();
		}
		if (!buildOptionString) {
			buildOptionString = "";
		}

		if (!taskDefinition) {
			taskDefinition = {
				type: FpcTaskProvider.FpcTaskType,
				file: file,

			};

		}

		// let option = new FpcTaskExecutionOptions(
		// 	undefined,
		// 	[]
		// );
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
				return  terminal;
				
			}) 
		);
	}


}

class FpcCustomExecution extends vscode.CustomExecution {

}
var diagCollection: vscode.DiagnosticCollection = vscode.languages.createDiagnosticCollection('fpc');

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
	

	buildend() {
		// The terminal has been closed. Shutdown the build.
		diagCollection.clear();
		let has_error: boolean = false;
		this.diagMaps.forEach((item, k) => {
			let uri = this.findFile(k);
			if (uri) {
				diagCollection.set(uri, item);
			} else {
				diagCollection.set(vscode.Uri.file(k), item);
			}
			if (!has_error) {
				item.forEach((d) => {
					if (d.severity === 0) {
						has_error = true;
					}
				});
			}
		});

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
				let f2 = e.substr(3);
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
				this.buildend();
				//This is a exitcode,not zero meens failure.
				this.closeEmitter.fire(code);
				
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
				return vscode.DiagnosticSeverity.Hint;
			default:
				return vscode.DiagnosticSeverity.Information;
		}
	}
	onOutput(lines: string) {
		let ls = <string[]>lines.split('\n');
		ls.forEach(line => {
			let reg = /^([\w]+\.(p|pp|pas|lpr|dpr|inc))\((\d+)\,(\d+)\)\s(Fatal|Error|Warning|Note):(.*)/;
			//reg.compile();

			let matchs = reg.exec(line);

			if (matchs) {
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));

				let ln = Number(matchs[3]);
				let col = Number(matchs[4]);
				let file = matchs[1];
				let level = matchs[5];
				let msg = matchs[6];
				// this.emit(
				// 	TerminalEscape.apply({ msg: file+"("+ln+','+col +") ", style: TE_Style.Blue })+
				// 	TerminalEscape.apply({ msg: level+":"+msg, style: TE_Style.Red })
				// );

				let diag = new vscode.Diagnostic(
					new vscode.Range(new vscode.Position(ln - 1, col - 1), new vscode.Position(ln - 1, col - 1)),
					msg,
					this.getDiagnosticSeverity(level)
				);

				if (this.diagMaps?.has(file)) {

					this.diagMaps.get(file)?.push(diag);
				} else {

					this.diagMaps.set(file, [diag]);

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