import * as vscode from 'vscode';
import * as ChildProcess from "child_process";
import * as util from './common/util';
import path = require('path');
import { configuration } from './common/configuration';
import { env } from 'process';
import * as fs from 'fs';

export class JediFormatter {
    private jcfpath:string;
    private default_cfg:string;
    private is_win:boolean=true;
    constructor() {
        const plat: NodeJS.Platform = process.platform;
        const arch = process.arch;
        let extensionProcessName = '';
    
        if (arch === 'x64') {
            if (plat === 'win32') {
                extensionProcessName = 'win32/jcf.exe';
            } else if (plat === 'linux') {
                extensionProcessName = 'x86_64-linux/jcf';
            } else if (plat == 'darwin') {
                extensionProcessName = 'x86_64-darwin/jcf';
            }
            else {
                throw "Invalid Platform";
            }
        } else if (arch === 'arm64') {
            if (plat === 'linux') {
                extensionProcessName = 'aarch64-linux/jcf';
            } else if (plat == 'darwin') {
                extensionProcessName = 'x86_64-darwin/jcf';
            }
            else if (plat == 'win32') {
                extensionProcessName = 'win32/jcf.exe';
            } else {
                throw "Invalid Platform";
            }
        } else {
            throw "Invalid arch";
        }
        this.is_win=plat==='win32';
        this.jcfpath = path.resolve(util.getExtensionFilePath("bin"), extensionProcessName);
        this.default_cfg=path.resolve(util.getExtensionFilePath("bin"),'jcfsettings.cfg');
        let cfg_path='';
        if(this.is_win){
            cfg_path=env.LOCALAPPDATA+'/lazarus/jcfsettings.cfg'; 
        }else{
            cfg_path=env.HOME+'/.lazarus/jcfsettings.cfg';
        };
        if(fs.existsSync(cfg_path)){
            this.default_cfg=cfg_path;
        }
        

    }
    getCfgConfig():string{
        let cfg=configuration.get<string>("format.cfgpath","");
        if(cfg==""){
            cfg=this.default_cfg;
        }
        return cfg;
    }
    doInit() {
        let _this = this;
        let enable=configuration.get<boolean>('format.enabled',true);
        if(!enable) return;
        // 👍 formatter implemented using API
        vscode.languages.registerDocumentFormattingEditProvider('objectpascal', {
            provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.ProviderResult<vscode.TextEdit[]> {
                let cfg_path=_this.getCfgConfig();
                let proc = ChildProcess.spawn(path.basename(_this.jcfpath), ['-inplace', '-y','-config='+cfg_path,'-F' ,document.fileName], { cwd: path.dirname(_this.jcfpath) });
                proc.stdout.on('data', (data) => {
                    console.log(`stdout: ${data}`);
                });

                proc.stderr.on('data', (data) => {
                    console.error(`stderr: ${data}`);
                });

                proc.on('close', (code) => {
                    console.log(`child process exited with code ${code}`);
                });
                return [];
            }
        });

    }
}

