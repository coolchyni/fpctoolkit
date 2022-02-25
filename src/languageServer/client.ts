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
    NotificationType,
    LanguageClient,
    ServerOptions,
    Executable,
    LanguageClientOptions,
    ShowMessageNotification,
    ShowMessageParams,
    CompletionRequest,
    MessageType
} from 'vscode-languageclient';
import { FpcProjectProvider } from '../providers/project';
import * as util from '../common/util';
import {InitializationOptions} from "./options";
import { error } from 'console';

interface InputRegion {
    startLine: number;
    endLine: number;
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
const InactiveRegionNotification: NotificationType<InactiveRegionParams, void> = new NotificationType<InactiveRegionParams, void>('pasls/inactiveRegions');


function getLanguageServerFileName(): string {
    let extensionProcessName: string = 'pasls';
    const plat: NodeJS.Platform = process.platform;
    const arch = process.arch;
    if (arch != 'x64') {
        throw "Invalid arch";
    }
    if (plat === 'win32') {
        extensionProcessName = 'x86_64-win64/pasls.exe';
    } else if (plat === 'linux') {
        extensionProcessName = 'x86_64-linux/pasls';
    } else if (plat == 'darwin') {
        extensionProcessName = 'x86_64-darwin/pasls';
    }
    else {
        throw "Invalid Platform";
    }
    return path.resolve(util.getExtensionFilePath("bin"), extensionProcessName);
}



interface myConfiguration extends vscode.WorkspaceConfiguration {
    cwd: string;
}
export class TLangClient {
    private client: LanguageClient | undefined;
    private inactiveRegionsDecorations = new Map<string, DecorationRangesPair>();
    constructor(
        public projProvider: FpcProjectProvider
    ) {
        this.client = undefined;
    };
    doInit() {
        //lsp

        console.log("Greetings from pascal-language-server 🙏");

        // Load the path to the language server from settings
        //let executable: string = workspace.getConfiguration('pascalLanguageServer').get("executable")!;
        let executable: string = getLanguageServerFileName();
        // TODO: download the executable for the active platform
        // https://github.com/genericptr/pascal-language-server/releases/download/x86_64-darwin/pasls
        // if (!executable) {
        // 	let target = 'x86_64-darwin';
        // 	executable = context.asAbsolutePath(path.join('bin', target, 'pasls'));
        // }

        console.log("executable: " + executable);

        // load environment variables from settings which are used for CodeTools
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


        let run: Executable = {
            command: executable,
            options: {
                env: userEnvironmentVariables
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
        let opt = this.projProvider.GetDefaultTaskOption();
        if (opt != undefined) {
           initializationOptions.updateByCompileOption(opt);
        }
        // client extensions configure their server
        let clientOptions: LanguageClientOptions = {
            initializationOptions: initializationOptions,
            // workspaceFolder: folder,
            documentSelector: [
                { scheme: 'file', language: 'objectpascal' },
                { scheme: 'untitled', language: 'objectpascal' }
            ]
        }

        this.client = new LanguageClient('fpctoolkit.lsp', 'Free Pascal Language Server', serverOptions, clientOptions);


        this.client.onReady().then(() => {
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
                        if ((e as any).hasFile){
                            let f=e.message.split('@')
                            let file=f[0].split(':')[0];
                            let pos=f[1].split(':');
                            vscode.window.showErrorMessage(e.message,'View Error').then(item=>{
                                if(item==='View Error'){
                                    vscode.workspace.openTextDocument(file).then(doc=>{
                                       
                                        let position:vscode.Position=new vscode.Position( Number.parseInt(pos[0])-1, Number.parseInt(pos[1])-1);
                                        vscode.window.showTextDocument(doc,{ selection: new vscode.Selection(position, position) } );
                                    });

                                }
                                
                            });
                            
                        }else{
                            vscode.window.showErrorMessage(e.message);
                        }
                      
                        
                        break;
                
                    default:
                        break;
                }
                

            });
            this.client?.onNotification(InactiveRegionNotification, (params: InactiveRegionParams) => {
                //const settings: CppSettings = new CppSettings(this.RootUri);
                const opacity: number | undefined = 0.5;//settings.inactiveRegionOpacity;
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
                        const newRange: vscode.Range = new vscode.Range(element.startLine, 0, element.endLine, 0);
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


            //todo 在注释里也会触发自动完成 ，需要定制 CompletionItemProvider
            //todo 在IFDEF 里的无效区域 要变灰 （服务端+客户端实现）
            //todo 文档改名或关闭 需要清除缓存 （服务端实现）

        });
    };
    public onDidChangeVisibleTextEditor(editor: vscode.TextEditor): void {

        // Apply text decorations to inactive regions
        const valuePair: DecorationRangesPair | undefined = this.inactiveRegionsDecorations.get(editor.document.uri.toString());
        if (valuePair) {
            editor.setDecorations(valuePair.decoration, valuePair.ranges); // VSCode clears the decorations when the text editor becomes invisible
        }

    }

    start() {
        this.client?.start();
    };
    stop() {
        this.client?.stop();
    }
}