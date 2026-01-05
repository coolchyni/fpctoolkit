/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as vscode from 'vscode';
import { CompileOption } from '../languageServer/options';
import * as ChildProcess from "child_process";
import path = require('path');
import { TerminalEscape, TE_Style } from '../common/escape';
import * as fs from 'fs';
import { client } from '../extension';
import { DiagnosticSeverity } from 'vscode';
import { LazarusBuildTerminal } from './lazarusBuildTerminal';
import { BaseBuildTerminal } from './baseBuildTerminal';

export class BuildOption {
	targetOS?: string;
	targetCPU?: string;
	customOptions?: string[];
	libPath?: string[];
	outputFile?: string;
	unitOutputDir?: string;
	optimizationLevel?: number;
	searchPath?: string[];
	syntaxMode?: string;
	forceRebuild?: boolean = false;
	msgIgnore?: Number[];
	cwd?: string;
	objectPath?: string;
	cleanExt?: string;
};

export class BuildEvent{
	before_build?: string[];
	after_build_success?:string[];
	after_build_failure?:string[];
}

export class FpcTaskDefinition implements vscode.TaskDefinition {
	[name: string]: any;
	readonly type: string = 'fpc';
	file?: string;
	cwd?: string;
	cleanExt?: string;
	inherited?: string;
	buildOption?: BuildOption;
	buildEvent?:BuildEvent;
}


export class FpcTaskProvider implements vscode.TaskProvider {
	static FpcTaskType = 'fpc';
	private defineMap: Map<string, FpcTaskDefinition> = new Map<string, FpcTaskDefinition>();
	public taskMap: Map<string, vscode.Task> = new Map<string, vscode.Task>();
	public GetTaskDefinition(name: string): FpcTaskDefinition | undefined {
		let result = this.defineMap.get(name);
		if (result && result.inherited) {
			let base = this.defineMap.get(result.inherited);
			if (base) {
				let realDefinition = new FpcTaskDefinition();
				this.mergeDefinition(base, realDefinition);
				this.mergeDefinition(result, realDefinition);
				return realDefinition;

			}
		}
		return result;
	}
	constructor(private workspaceRoot: string, private cwd: string | undefined = undefined) {
	}

	public async clean() {
		this.defineMap.clear();
		this.taskMap.clear();
	}
	public async provideTasks(): Promise<vscode.Task[]> {
		return this.getTasks();
	}

	public resolveTask(_task: vscode.Task): vscode.Task | undefined {
		if (this.taskMap.has(_task.name)) {
			let task = this.taskMap.get(_task.name);
			task!.definition = _task.definition;
			return task;
		} else {
			const file: string = _task.definition.file;
			if (file) {
				const definition: FpcTaskDefinition = <any>_task.definition;
				if (_task.definition.cwd) {
					this.cwd = this.workspaceRoot + '/' + _task.definition.cwd;
				}
				let task = this.getTask(_task.name, definition.file, definition);
				this.taskMap.set(_task.name, task);
				return task;
			}
		}

		return undefined;
	}

	private async getTasks(): Promise<vscode.Task[]> {
		return [];
	}
	private mergeDefinition(from: FpcTaskDefinition, to: FpcTaskDefinition) {
		to.file = to.file ?? from.file;
		to.cwd = to.cwd ?? from.cwd;
		to.cleanExt = to.cleanExt ?? from.cleanExt;
		if (from.buildOption != undefined) {
			if (to.buildOption === undefined) {
				to.buildOption = Object.assign({}, from.buildOption);
			}
			else {
				to.buildOption.customOptions = ([] as string[]).concat(from.buildOption.customOptions ?? [], to.buildOption.customOptions ?? []);
				to.buildOption.libPath = ([] as string[]).concat(from.buildOption.libPath ?? [], to.buildOption.libPath ?? []);
				to.buildOption.searchPath = ([] as string[]).concat(from.buildOption.searchPath ?? [], to.buildOption.searchPath ?? []);
				to.buildOption.msgIgnore = ([] as Number[]).concat(from.buildOption.msgIgnore ?? [], to.buildOption.msgIgnore ?? []);

				to.buildOption.optimizationLevel = to.buildOption.optimizationLevel ?? from.buildOption.optimizationLevel;
				to.buildOption.outputFile = to.buildOption.outputFile ?? from.buildOption.outputFile;
				to.buildOption.syntaxMode = to.buildOption.syntaxMode ?? from.buildOption.syntaxMode;
				to.buildOption.targetCPU = to.buildOption.targetCPU ?? from.buildOption.targetCPU;
				to.buildOption.targetOS = to.buildOption.targetOS ?? from.buildOption.targetOS;
				to.buildOption.unitOutputDir = to.buildOption.unitOutputDir ?? from.buildOption.unitOutputDir;
				to.buildOption.forceRebuild = to.buildOption.forceRebuild ?? from.buildOption.forceRebuild;

			}
		}

	}
	public getTask(name: string, file?: string, definition?: FpcTaskDefinition): vscode.Task {
		// if (definition?.inherited) {
		// 	let pdefine = this.defineMap.get(definition.inherited);
		// 	if (pdefine) {
		// 		let realDefinition = new FpcTaskDefinition();
		// 		this.mergeDefinition(definition, realDefinition);
		// 		this.mergeDefinition(pdefine, realDefinition);
		// 		this.defineMap.set(name, realDefinition);
		// 		let task = new FpcTask(this.cwd ? this.cwd : this.workspaceRoot, name, file!, definition, realDefinition);
		// 		return task;
		// 	}

		// }
		this.defineMap.set(name, definition!);
		let task = new FpcTask(this.cwd ? this.cwd : this.workspaceRoot, name, file!, definition!);


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

	public refresh() {
		client.restart();
	}
}

export enum BuildMode {
	normal,
	rebuild
}
export class FpcTask extends vscode.Task {
	private _BuildMode: BuildMode = BuildMode.normal;
	public get BuildMode(): BuildMode {
		return this._BuildMode;
	}
	public set BuildMode(value: BuildMode) {
		this._BuildMode = value;
	}
	constructor(cwd: string, name: string, file: string, taskDefinition: FpcTaskDefinition) {

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

				//taskProvider.GetTaskDefinition()
				let buildOptionString: string = '';
				let realDefinition=taskProvider.GetTaskDefinition(name);
				if (realDefinition === undefined) {
					realDefinition = taskDefinition;
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
				buildOptionString += '-vq '; //show message numbers 

				let fpcpath = process.env['PP'];//  configuration.get<string>('env.PP');
				if (fpcpath === '') {
					fpcpath = 'fpc';
				}

				// Determine if this is a Lazarus project
				const isLazarusProject = taskDefinition?.isLazarusProject;
				
				let terminal: FpcBuildTaskTerminal | LazarusBuildTerminal;
				
				if (isLazarusProject) {
					// Use Lazarus build terminal for .lpi/.lpr files
					const buildMode = (taskDefinition as any).buildMode || name;
					terminal = new LazarusBuildTerminal(cwd, fpcpath!, taskDefinition?.lazarusProjectFile, buildMode);
					(terminal as LazarusBuildTerminal).forceRebuild = this._BuildMode === BuildMode.rebuild;
				} else {
					// Use standard FPC terminal for other files
					terminal = new FpcBuildTaskTerminal(cwd, fpcpath!);
				}
				
				if(taskDefinition.buildEvent){
					if(taskDefinition.buildEvent.before_build){
						let commands=taskDefinition.buildEvent.before_build;
						terminal.event_before_build=()=>{
							for (const cmd of commands) {
								let result=ChildProcess.execSync(cmd);
								terminal.emit(result.toString())
							}
						}
					}
					if(taskDefinition.buildEvent.after_build_failure || taskDefinition.buildEvent.after_build_success){
						let commands_failure=taskDefinition.buildEvent.after_build_failure;
						let commands_success=taskDefinition.buildEvent.after_build_success;
						terminal.event_after_build=(success)=>{
							if(success && commands_success){
								for (const cmd of commands_success) {
									let result=ChildProcess.execSync(cmd);
									terminal.emit(result.toString())
								}
							}else if(commands_failure)
								for (const cmd of commands_failure) {
									let result=ChildProcess.execSync(cmd);
									terminal.emit(result.toString())
								}
							
							}
							
					}

				}
				
				// Set arguments based on terminal type
				if (terminal instanceof LazarusBuildTerminal) {
					// For Lazarus projects, the terminal handles compilation strategy internally
					terminal.args = `${taskDefinition?.file} ${buildOptionString}`.split(' ');
				} else {
					// For FPC projects, use traditional approach
					terminal.args = `${taskDefinition?.file} ${buildOptionString}`.split(' ');
					if (this._BuildMode == BuildMode.rebuild) {
						terminal.args.push('-B');
					}
				}
				return terminal;

			})
		);
		//this.TaskBuildOptionString = buildOptionString;
	}


}

class FpcCustomExecution extends vscode.CustomExecution {

}
export var diagCollection: vscode.DiagnosticCollection = vscode.languages.createDiagnosticCollection('fpc');

class FpcBuildTaskTerminal extends BaseBuildTerminal {
	constructor(cwd: string, fpcpath: string) {
		super(cwd, fpcpath);
	}

	protected async executeBuild(): Promise<number> {
		return new Promise<number>((resolve) => {
			this.emit(TerminalEscape.apply({ msg: `${this.fpcpath} ${this.args.join(' ')}\r\n`, style: [TE_Style.Bold] }));
			this.process = ChildProcess.spawn(this.fpcpath, this.args, { cwd: this.cwd });

			this.process.stdout?.on('data', this.stdout.bind(this));
			this.process.stderr?.on('data', this.stderr.bind(this));
			this.process.on('close', async (code) => {
				await this.handleProcessClose(code);
				resolve(code || 0);
			});
		});
	}

	private stdout(data: any) {
		if (typeof data === "string") {
			this.buffer += data;
		} else {
			this.buffer += data.toString("utf8");
		}
		const end = this.buffer.lastIndexOf('\n');
		if (end !== -1) {
			this.onOutput(this.buffer.substr(0, end));
			this.buffer = this.buffer.substr(end + 1);
		}
	}

	private onOutput(lines: string) {
		const ls = lines.split('\n');
		
		ls.forEach(line => {
			line = line.trim();
			if (!line) { return; }

			// Try to parse FPC-style error or "Compiling" context
			if (this.parseFpcStyleError(line)) {
				return;
			}

			// Handle other error/warning lines that don't match the standard format
			if (line.startsWith('Error:') || line.startsWith('Fatal:')) {
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
			} else if (line.startsWith('Warning:')) {
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.BrightYellow] }));
			} else if (line.startsWith('Note:') || line.startsWith('Hint:')) {
				this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));
			} else {
				this.emit(line);
			}
		});
	}
}

export let taskProvider: FpcTaskProvider;

if (vscode.workspace.workspaceFolders) {
	const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;
	taskProvider = new FpcTaskProvider(workspaceRoot);
}

