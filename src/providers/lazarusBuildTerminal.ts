import * as vscode from 'vscode';
import * as ChildProcess from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import { TerminalEscape, TE_Style } from '../common/escape';
import { LazarusCompiler, CompilerResult } from './lazarusCompiler';
import { diagCollection } from './task';
import { BaseBuildTerminal } from './baseBuildTerminal';

export class LazarusBuildTerminal extends BaseBuildTerminal {
    public projectFile: string = '';
    public buildMode: string = '';
    public forceRebuild: boolean = false;

    constructor(
        cwd: string, 
        fpcpath: string,
        projectFile?: string,
        buildMode?: string
    ) {
        super(cwd, fpcpath);
        this.projectFile = projectFile || '';
        this.buildMode = buildMode || '';
    }

    protected async executeBuild(): Promise<number> {
        return new Promise<number>(async (resolve) => {
            try {
                // Resolve project file path
                const projectFilePath = path.isAbsolute(this.projectFile) 
                    ? this.projectFile 
                    : path.join(this.cwd, this.projectFile);

                // Determine compilation strategy
                const strategy = await LazarusCompiler.getBestCompilationStrategy(
                    projectFilePath,
                    this.buildMode,
                    this.cwd,
                    this.fpcpath,
                    this.args,
                    this.forceRebuild
                );

                // Display compilation command
                this.emit(TerminalEscape.apply({ 
                    msg: `Using ${strategy.compiler} compiler\r\n`, 
                    style: [TE_Style.Bold, TE_Style.Green] 
                }));
                
                this.emit(TerminalEscape.apply({ 
                    msg: `${strategy.command} ${strategy.args.join(' ')}\r\n`, 
                    style: [TE_Style.Bold] 
                }));

                // Execute compilation
                this.process = ChildProcess.spawn(strategy.command, strategy.args, { cwd: this.cwd });

                this.process.stdout?.on('data', (data) => this.handleOutput(data, strategy.compiler));
                this.process.stderr?.on('data', (data) => this.handleError(data, strategy.compiler));
                
                this.process.on('close', async (code) => {
                    await this.handleProcessClose(code);
                    resolve(code || 0);
                });

            } catch (error) {
                this.emit(TerminalEscape.apply({ 
                    msg: `Compilation failed: ${error}\r\n`, 
                    style: [TE_Style.Red] 
                }));
                
                this.reason = vscode.TerminalExitReason.Unknown;
                resolve(1);
            }
        });
    }

    private handleOutput(data: any, compiler: 'lazbuild' | 'fpc') {
        const output = typeof data === "string" ? data : data.toString("utf8");
        
        if (compiler === 'lazbuild') {
            this.handleLazbuildOutput(output);
        } else {
            this.handleFpcOutput(output);
        }
    }

    private handleError(data: any, compiler: 'lazbuild' | 'fpc') {
        this.stderr(data);
    }

    private handleLazbuildOutput(data: string) {
        this.buffer += data;
        const end = this.buffer.lastIndexOf('\n');
        if (end !== -1) {
            this.processLazbuildLines(this.buffer.substr(0, end));
            this.buffer = this.buffer.substr(end + 1);
        }
    }

    private handleFpcOutput(data: string) {
        this.buffer += data;
        const end = this.buffer.lastIndexOf('\n');
        if (end !== -1) {
            this.processFpcLines(this.buffer.substr(0, end));
            this.buffer = this.buffer.substr(end + 1);
        }
    }

    private processLazbuildLines(lines: string) {
        const lineArray = lines.split('\n');
        
        for (let line of lineArray) {
            line = line.trim();
            if (!line) { continue; }

            // Try to parse FPC-style error or "Compiling" context
            if (this.parseFpcStyleError(line)) {
                continue;
            }

            // Parse output for other errors and warnings
            if (line.includes('Error:') || line.includes('Fatal:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
            } else if (line.includes('Warning:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.BrightYellow] }));
            } else if (line.includes('Note:') || line.includes('Hint:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));
            } else {
                this.emit(line);
            }
        }
    }

    private processFpcLines(lines: string) {
        this.processLazbuildLines(lines);
    }
}