import * as vscode from 'vscode';
import * as ChildProcess from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import { TerminalEscape, TE_Style } from '../common/escape';
import { LazarusCompiler, CompilerResult } from './lazarusCompiler';
import { diagCollection } from './task';

export class LazarusBuildTerminal implements vscode.Pseudoterminal, vscode.TerminalExitStatus {
    private writeEmitter = new vscode.EventEmitter<string>();
    onDidWrite: vscode.Event<string> = this.writeEmitter.event;
    private closeEmitter = new vscode.EventEmitter<number>();
    onDidClose: vscode.Event<number> = this.closeEmitter.event;

    public event_before_build?: () => void;
    public event_after_build?: (success: boolean) => void;

    private process?: ChildProcess.ChildProcess;
    protected buffer: string = "";
    protected errbuf: string = "";

    private diagMaps: Map<string, vscode.Diagnostic[]>;
    public args: string[] = [];
    public projectFile: string = '';
    public buildMode: string = '';
    public forceRebuild: boolean = false;
    reason: vscode.TerminalExitReason = vscode.TerminalExitReason.Unknown;
    code: number | undefined;

    constructor(
        private cwd: string, 
        private fpcpath: string,
        projectFile?: string,
        buildMode?: string
    ) {
        this.diagMaps = new Map<string, vscode.Diagnostic[]>();
        this.projectFile = projectFile || '';
        this.buildMode = buildMode || '';
        
        this.onDidClose((e) => {
            // Handle close event
        });
    }

    clear() {
        // Clear implementation
    }

    open(initialDimensions: vscode.TerminalDimensions | undefined): void {
        this.doBuild();
    }

    close(): void {
        // Close implementation
    }

    private async doBuild(): Promise<number> {
        return new Promise<number>(async (resolve) => {
            this.buffer = "";
            this.errbuf = "";
            this.diagMaps.clear();

            // Create output directories if needed
            this.createOutputDirectories();

            // Execute before build event
            if (this.event_before_build) {
                this.event_before_build();
            }

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
                
                this.process.on('close', (code) => {
                    this.writeEmitter.fire(`Exited with code ${code}.\r\nBuild complete. \r\n\r\n`);
                    
                    // Set exit reason
                    if (code === 0) {
                        this.reason = vscode.TerminalExitReason.User;
                    } else {
                        this.reason = vscode.TerminalExitReason.Unknown;
                    }
                    
                    this.buildend().then(() => {
                        this.closeEmitter.fire(code || 0);
                    });
                    
                    // Execute after build event
                    if (this.event_after_build) {
                        this.event_after_build(code === 0);
                    }
                    
                    resolve(code || 0);
                });

            } catch (error) {
                this.emit(TerminalEscape.apply({ 
                    msg: `Compilation failed: ${error}\r\n`, 
                    style: [TE_Style.Red] 
                }));
                
                this.reason = vscode.TerminalExitReason.Unknown;
                this.closeEmitter.fire(1);
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
        const output = typeof data === "string" ? data : data.toString("utf8");
        this.emit(TerminalEscape.apply({ msg: output, style: [TE_Style.Yellow] }));
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
        
        for (const line of lineArray) {
            // Parse lazbuild output for errors and warnings
            // lazbuild typically shows compilation progress and errors
            if (line.includes('Error:') || line.includes('Fatal:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
            } else if (line.includes('Warning:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.BrightYellow] }));
            } else if (line.includes('Note:') || line.includes('Hint:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));
            } else {
                this.emit(line);
            }
            
            // Try to parse FPC-style error messages that might be embedded in lazbuild output
            this.parseFpcStyleError(line);
        }
    }

    private processFpcLines(lines: string) {
        const lineArray = lines.split('\n');
        
        for (const line of lineArray) {
            this.parseFpcStyleError(line);
            
            if (line.startsWith('Error:') || line.startsWith('Fatal:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
            } else if (line.startsWith('Warning:')) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.BrightYellow] }));
            } else {
                this.emit(line);
            }
        }
    }

    private parseFpcStyleError(line: string) {
        // Parse FPC-style error messages
        const reg = /^(([-:\w\\\/]+)\.(p|pp|pas|lpr|dpr|inc))\(((\d+)(\,(\d+))?)\)\s(Fatal|Error|Warning|Note|Hint): \((\d+)\) (.*)/;
        const matches = reg.exec(line);

        if (matches) {
            const ln = Number(matches[5]);
            const col = Number(matches[7]) || 1;
            const file = matches[1];
            const unit = matches[2];
            const level = matches[8];
            const msgcode = matches[9];
            const msg = matches[10];

            const diag = new vscode.Diagnostic(
                new vscode.Range(new vscode.Position(ln - 1, col - 1), new vscode.Position(ln - 1, col - 1)),
                msg,
                this.getDiagnosticSeverity(level)
            );
            diag.code = Number.parseInt(msgcode);

            const basename = path.basename(file);
            if (this.diagMaps?.has(basename)) {
                this.diagMaps.get(basename)?.push(diag);
            } else {
                this.diagMaps.set(basename, [diag]);
            }

            if (diag.severity === vscode.DiagnosticSeverity.Error) {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Red] }));
            } else {
                this.emit(TerminalEscape.apply({ msg: line, style: [TE_Style.Cyan] }));
            }
        }
    }

    private getDiagnosticSeverity(level: string): vscode.DiagnosticSeverity {
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

    private createOutputDirectories() {
        // Create output directories from FPC args
        const outputFileArg = this.args.find(arg => arg.startsWith('-o'));
        if (outputFileArg) {
            let outfile = outputFileArg.substring(2).trim();
            if (outfile.startsWith('.')) {
                outfile = path.join(this.cwd, outfile);
            }
            const dir = path.dirname(outfile);
            if (!fs.existsSync(dir)) {
                try {
                    fs.mkdirSync(dir, { recursive: true });
                } catch (error) {
                    vscode.window.showErrorMessage("Can't create output directory.(" + dir + ")");
                }
            }
        }

        const unitOutputDirArg = this.args.find(arg => arg.startsWith('-FU'));
        if (unitOutputDirArg) {
            let dir = unitOutputDirArg.substring(3).trim();
            if (dir.startsWith('.')) {
                dir = path.join(this.cwd, dir);
            }
            if (!fs.existsSync(dir)) {
                try {
                    fs.mkdirSync(dir, { recursive: true });
                } catch (error) {
                    vscode.window.showErrorMessage("Can't create unit output directory.(" + dir + ")");
                }
            }
        }
    }

    private async buildend() {
        diagCollection.clear();
        let has_error: boolean = false;

        for (const iter of this.diagMaps) {
            const key = iter[0];
            const item = iter[1];
            let uri: vscode.Uri | undefined = undefined;

            if (fs.existsSync(key)) {
                uri = vscode.Uri.file(key);
            } else {
                uri = this.findFile(key);
            }

            if (uri) {
                diagCollection.set(uri, item);
            } else {
                diagCollection.set(vscode.Uri.file(key), item);
            }

            if (!has_error) {
                item.forEach((d) => {
                    if (d.severity === vscode.DiagnosticSeverity.Error) {
                        has_error = true;
                    }
                });
            }
        }

        if (has_error) {
            vscode.commands.executeCommand('workbench.actions.view.problems');
        }
    }

    private findFile(filename: string): vscode.Uri | undefined {
        let f = path.join(this.cwd, filename);
        if (fs.existsSync(f)) {
            return vscode.Uri.file(f);
        }

        for (const arg of this.args) {
            if (arg.startsWith('-Fu')) {
                let f2 = arg.substring(3);
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

    emit(msg: string) {
        this.writeEmitter.fire(msg + '\r\n');
    }
}