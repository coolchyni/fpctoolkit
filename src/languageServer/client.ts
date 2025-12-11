/**
 * @File   : client.ts
 * @Author :  (coolchyni)
 * @Link   : 
 * @Date   : 2/16/2022, 11:26:06 PM
 */
import path = require('path');
import * as vscode from 'vscode';
import { workspace } from 'vscode';
import {
    State,
    NotificationType,
    LanguageClient,
    ServerOptions,
    Executable,
    LanguageClientOptions,
    ShowMessageNotification,
    ShowMessageParams,
    MessageType,
    ExecuteCommandRequest,
    ExecuteCommandParams,
    ErrorHandler,
    Message,
    ErrorHandlerResult,
    ErrorAction,
    CloseHandlerResult,
    CloseAction,
    Trace} from 'vscode-languageclient/node';

import { FpcProjectProvider } from '../providers/project';
import * as util from '../common/util';
import { CompileOption, InitializationOptions } from "./options";
import { env } from 'process';
import * as fs from 'fs-extra';
import { client, logger } from '../extension';
import { ClientRequest } from 'http';

interface InputRegion {
    startLine: number;
    startCol:number;
    endLine: number;
    endCol:number;
}

interface DecorationRangesPair {
    decoration: vscode.TextEditorDecorationType;
    ranges: vscode.Range[];
}

interface InactiveRegionParams {
    uri: string;
    fileVersion: number;
    regions: InputRegion[];
}

// Notifications from the server
const InactiveRegionNotification: NotificationType<InactiveRegionParams> = new NotificationType<InactiveRegionParams>('pasls.inactiveRegions');

//set cursor pos
interface SetSelectionParams {
    uri: string;
    /**
    * The position at which the selection starts.
    * This position might be before or after {@link Selection.active active}.
    */
    anchor: vscode.Position;

    /**
     * The position of the cursor.
     * This position might be before or after {@link Selection.anchor anchor}.
     */
    active: vscode.Position;
}
const SetSelectionNotification: NotificationType<SetSelectionParams> = new NotificationType<SetSelectionParams>('pasls/setSelection');

function GetEnvironmentVariables(): {} {
    // load environment variables from settings which are used for CodeTools
    const plat = process.platform;
    let userEnvironmentVariables = {};
    let keys: string[] = ['PP', 'FPCDIR', 'LAZARUSDIR', 'FPCTARGET', 'FPCTARGETCPU'];
    let settingEnvironmentVariables = workspace.getConfiguration('fpctoolkit.env');
    Object.keys(settingEnvironmentVariables).forEach(key => {
        if (keys.includes(key)) {
            if (settingEnvironmentVariables[key]) {
                userEnvironmentVariables[key] = settingEnvironmentVariables[key];
            }
        }
    });
    //set default value
    let PP = settingEnvironmentVariables.get<string>('PP');
    if (PP === undefined || PP === '') //not init
    {
        if (plat === 'win32') {
            ///3.2.2/bin/i386-win32/fpc.exe
            //search lazarus
            let dirs = ['C:/lazarus/fpc', 'C:/FPC'];
            let ver_test = /\d+\.\d+\.\d+/;
            for (const _dir of dirs) {
                if (fs.pathExistsSync(_dir)) {
                    let subdirs = fs.readdirSync(_dir);
                    for (const fpcver of subdirs) {
                        if (ver_test.test(fpcver)) { //found it 
                            if (_dir.startsWith('C:/lazarus')) {
                                userEnvironmentVariables['LAZARUSDIR'] = 'C:/lazarus';
                                env['LAZARUSDIR'] = userEnvironmentVariables['LAZARUSDIR'];
                            }
                            userEnvironmentVariables['PP'] = path.join(_dir, fpcver, 'bin', 'i386-win32', 'fpc.exe');
                            userEnvironmentVariables['FPCDIR'] = path.join(_dir, fpcver, 'source');
                            env['PP'] = userEnvironmentVariables['PP'];

                            return userEnvironmentVariables;
                        }
                    }
                }
            }
        } else {
            let dirs = ['/usr/bin/fpc', '/usr/local/bin/fpc'];
            let ver_test = new RegExp('\d+\.\d+\.\d+');
            for (const _dir of dirs) {
                if (fs.existsSync(_dir)) {
                    userEnvironmentVariables['PP'] = _dir;
                }
            }
            if (fs.existsSync('/usr/local/share/fpcsrc')) {
                userEnvironmentVariables['FPCDIR'] = '/usr/local/share/fpcsrc';
            }
        }
    }

    env['PP'] = userEnvironmentVariables['PP'];
    env['LAZARUSDIR'] = userEnvironmentVariables['LAZARUSDIR'];

    return userEnvironmentVariables;
}
interface myConfiguration extends vscode.WorkspaceConfiguration {
    cwd: string;
}
export class TLangClient implements ErrorHandler  {
    private client: LanguageClient | undefined;
    private targetOS?: string;
    private targetCPU?: string;
    private inactiveRegionsDecorations = new Map<string, DecorationRangesPair>();
    constructor(
        public projProvider: FpcProjectProvider
    ) {
        this.client = undefined;
    };

      /**
     * An error has occurred while writing or reading from the connection.
     *
     * @param error - the error received
     * @param message - the message to be delivered to the server if know.
     * @param count - a count indicating how often an error is received. Will
     *  be reset if a message got successfully send or received.
     */
    error(error: Error, message: Message | undefined, count: number | undefined): ErrorHandlerResult{
        logger.appendLine(error.name+' '+error.message);
        return  {action:ErrorAction.Continue} as ErrorHandlerResult;
    }
    /**
    * The connection to the server got closed.
    */
    closed(): CloseHandlerResult{
        logger.appendLine("Server closed.");
        return  {action:CloseAction.Restart} as CloseHandlerResult;
    }

    private getLanguageServerFileName(): string {
        let extensionProcessName: string = 'pasls';
        let paslspath=vscode.workspace.getConfiguration('fpctoolkit.pasls').get<string>('path');
      

        const plat: NodeJS.Platform = process.platform;
        const arch = process.arch;
        if (arch === 'x64') {
            this.targetCPU = 'x86_64';
            if (plat === 'win32') {            // 检查是否有调试器附加
                if(process.env.DEBUG_MODE==='true'){
                    logger.appendLine("Debug mode detected, using paslsproxy.exe");
                    extensionProcessName = 'debug/paslsproxy.exe';
                }else{
                    extensionProcessName = 'win32/pasls.exe';
                }
                this.targetOS = 'win64';
            } else if (plat === 'linux') {
                extensionProcessName = 'x86_64-linux/pasls';
                this.targetOS = 'linux';
            } else if (plat == 'darwin') {
                extensionProcessName = 'x86_64-darwin/pasls';
                this.targetOS = 'darwin';
            }
            else {
                throw "Invalid Platform";
            }
        } else if (arch === 'arm64') {
            this.targetCPU = 'aarch64';
            if (plat === 'linux') {
                extensionProcessName = 'aarch64-linux/pasls';
                this.targetOS = 'linux';
            } else if (plat == 'darwin') {
                extensionProcessName = 'x86_64-darwin/pasls';
                this.targetOS = 'darwin';
            }
            else if (plat == 'win32') {
                this.targetOS = 'win32';
                if(process.env.DEBUG_MODE==='true'){
                    logger.appendLine("Debug mode detected, using paslsproxy.exe");
                    extensionProcessName = 'debug/paslsproxy.exe';
                }else{
                    extensionProcessName = 'win32/pasls.exe';
                }
            } else {
                throw "Invalid Platform";
            }
        } else {
            throw "Invalid arch";
        }
        if(paslspath && paslspath.length>0){
            return paslspath;
        }
        return path.resolve(util.getExtensionFilePath("bin"), extensionProcessName);
    };
    async doOnReady() {
        this.client?.onNotification(ShowMessageNotification.type, (e: ShowMessageParams) => {
            //vscode.window.showErrorMessage(e.message);

            switch (e.type) {
                case MessageType.Info:
                    vscode.window.showInformationMessage(e.message);
                    break;
                case MessageType.Warning:
                    vscode.window.showWarningMessage(e.message);
                    break;
                case MessageType.Error:
                    if ((e as any).hasFile) {
                        let f = e.message.split('@')
                        let file = f[0].split(' ')[0];
                        let pos = f[1].split(':');

                        let position: vscode.Position = new vscode.Position(Number.parseInt(pos[0]), Number.parseInt(pos[1]));

                        let diag=new vscode.Diagnostic(new vscode.Range(position,position),e.message);
                    
                        this.client?.diagnostics?.set( vscode.Uri.parse(file),[diag]);
                    

                        vscode.window.showErrorMessage(e.message, 'View Error').then(item => {
                            if (item === 'View Error') {
                                vscode.workspace.openTextDocument(file).then(doc => {

                                    let position: vscode.Position = new vscode.Position(Number.parseInt(pos[0]), Number.parseInt(pos[1]));
                                    vscode.window.showTextDocument(doc, { selection: new vscode.Selection(position, position) });
                                });

                            }

                        });

                    } else {
                        //logger.appendLine(e.message);
                        vscode.window.showErrorMessage(e.message);
                    }


                    break;

                default:
                    break;
            }


        });
        this.client?.onNotification(InactiveRegionNotification, (params: InactiveRegionParams) => {
            //const settings: CppSettings = new CppSettings(this.RootUri);
            const opacity: number | undefined = 0.3;//settings.inactiveRegionOpacity;
            if (opacity !== null && opacity !== undefined) {
                let backgroundColor: string | undefined = "";//settings.inactiveRegionBackgroundColor;
                if (backgroundColor === "") {
                    backgroundColor = undefined;
                }
                let color: string | undefined = "";//settings.inactiveRegionForegroundColor;
                if (color === "") {
                    color = undefined;
                }
                const decoration: vscode.TextEditorDecorationType = vscode.window.createTextEditorDecorationType({
                    opacity: opacity.toString(),
                    backgroundColor: backgroundColor,
                    color: color,
                    rangeBehavior: vscode.DecorationRangeBehavior.OpenOpen
                });
                // We must convert to vscode.Ranges in order to make use of the API's
                const ranges: vscode.Range[] = [];
                params.regions.forEach(element => {
                    const newRange: vscode.Range = new vscode.Range(element.startLine-1, element.startCol-1, element.endLine-1, element.endCol-1);
                    ranges.push(newRange);
                });
                // Find entry for cached file and act accordingly
                const valuePair: DecorationRangesPair | undefined = this.inactiveRegionsDecorations.get(params.uri);
                if (valuePair) {
                    // Disposing of and resetting the decoration will undo previously applied text decorations
                    valuePair.decoration.dispose();
                    valuePair.decoration = decoration;
                    // As vscode.TextEditor.setDecorations only applies to visible editors, we must cache the range for when another editor becomes visible
                    valuePair.ranges = ranges;
                } else { // The entry does not exist. Make a new one
                    const toInsert: DecorationRangesPair = {
                        decoration: decoration,
                        ranges: ranges
                    };
                    this.inactiveRegionsDecorations.set(params.uri, toInsert);
                }
                //if (settings.dimInactiveRegions && params.fileVersion === openFileVersions.get(params.uri)) {
                // Apply the decorations to all *visible* text editors
                const editors: vscode.TextEditor[] = vscode.window.visibleTextEditors.filter(e => e.document.uri.toString() === params.uri);
                for (const e of editors) {
                    e.setDecorations(decoration, ranges);
                }
                //}
            }

        });

        this.client?.onNotification(SetSelectionNotification, (params: SetSelectionParams) => {
            let uri=vscode.Uri.parse(params.uri);
            vscode.workspace.openTextDocument(uri).then(doc => {
                setTimeout(() => {
                    vscode.window.showTextDocument(doc,  { selection: new vscode.Selection(params.anchor, params.active) });
                }, 500);
            });
        });

    }
    async doInit() {
        //lsp

        console.log("Greetings from pascal-language-server 🙏");

        // Load the path to the language server from settings
        //let executable: string = workspace.getConfiguration('pascalLanguageServer').get("executable")!;
        let executable: string = this.getLanguageServerFileName();
        if(process.platform!='win32'){
            fs.chmod(executable,755);
        }
        // TODO: download the executable for the active platform
        // https://github.com/genericptr/pascal-language-server/releases/download/x86_64-darwin/pasls
        // if (!executable) {
        // 	let target = 'x86_64-darwin';
        // 	executable = context.asAbsolutePath(path.join('bin', target, 'pasls'));
        // }

        console.log("executable: " + executable);

        let run: Executable = {
            command: executable,
            options: {
                env: GetEnvironmentVariables()
            }
        };
        let debug: Executable = run;

        let serverOptions: ServerOptions = {
            run,
            debug
        };

        //Connect to language server via socket

        //rpcjson
        // The server is a started as a separate app and listens on port 5007
        //  let connectionInfo = {
        //     port: 9999
        // };
        // let serverOptions = () => {

        //     let socket = net.connect(connectionInfo);
        // 	socket.on("data",(data:Buffer)=>{
        // 		console.info(data.toString());
        // 	});

        //     let result: StreamInfo = {
        //         writer: socket,
        //         reader: socket
        //     };
        //     return Promise.resolve(result);
        // };

        var initializationOptions = new InitializationOptions();

        let opt = await this.projProvider.GetDefaultTaskOption();
        if (opt != undefined) {
            initializationOptions.updateByCompileOption(opt);
        } else {
            opt = new CompileOption();
            opt.buildOption!.targetCPU = this.targetCPU;
            opt.buildOption!.targetOS = this.targetOS;
        }

        // client extensions configure their server
        let clientOptions: LanguageClientOptions = {
            initializationOptions: initializationOptions,
            errorHandler: this,
            // workspaceFolder: folder,
            documentSelector: [
                { scheme: 'file', language: 'objectpascal' },
                { scheme: 'untitled', language: 'objectpascal' }
            ]
        }

        this.client = new LanguageClient('fpctoolkit.lsp', 'Free Pascal Language Server', serverOptions, clientOptions);
  
    };
    public onDidChangeVisibleTextEditor(editor: vscode.TextEditor): void {

        // Apply text decorations to inactive regions
        const valuePair: DecorationRangesPair | undefined = this.inactiveRegionsDecorations.get(editor.document.uri.toString());
        if (valuePair) {
            editor.setDecorations(valuePair.decoration, valuePair.ranges); // VSCode clears the decorations when the text editor becomes invisible
        }

    }

    async start(): Promise<void> {
        await this.client?.start();
        await this.doOnReady();
    };
    async stop(): Promise<void> {
        if (!this.client) {
            return;
        }
        try {
            await this.client.stop(10000);
        } catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            logger.appendLine(`Failed to stop language client: ${message}`);
        }
    };

    async restart(): Promise<void> {

        await this.stop();
        await this.doInit();
        await this.client?.start();
        await this.doOnReady();

    };

    async doCodeComplete(editor:vscode.TextEditor): Promise<void> {
        var req:ExecuteCommandParams={
            command:"pasls.completeCode",
            arguments:[
                editor.document.uri.toString(),
                editor.selection.start            ]
        };
        await this.client?.sendRequest(ExecuteCommandRequest.type,req);       

    }

    async getUnitPath( unitnames:string[]): Promise<string[]> {

        var req:ExecuteCommandParams={
            command:"pasls.getUnitPath",
            arguments:unitnames
        };
        let result:ExecuteCommandParams=await this.client?.sendRequest(ExecuteCommandRequest.type,req);


        if(result &&  result.arguments){
            return result.arguments;
        }else{
            return [];
        }
        
    }
}
