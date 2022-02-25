import internal = require('stream');
import * as vscode from 'vscode';
import *  as fs from 'fs';
import * as path from 'path';
import {configuration} from '../common/configuration';
export class CompileOption {
    /**
     * Compile Option
     */
    public type:string="fpc";
    public cwd:string="";

    public presentation={
        showReuseMessage: false,
		clear: true,
        revealProblems: "onProblem"
    };
            
    public buildOption?:{
         targetOS?: string,
         targetCPU?: string,
         customOptions?:string[],    
         libPath?: string[],
         outputFile?: string,
         unitOutputDir?: string,
         optimizationLevel?: number,
         searchPath?: string[],
         syntaxMode?: string
         
    };
   
    constructor(
        public label:string,
        public file: string, 
    ) {
        this.buildOption={
            unitOutputDir:"./out",
            customOptions:[
                "-dDEBUG"
            ]
        };
       
    }
    
    toOptionString() {
        let fpccfg = configuration;
        let globalOption={
            customOptions: fpccfg.get<string[]>('customOptions'),    
            libPath: fpccfg.get<string[]>('libPath'), 
            searchPath:fpccfg.get<string[]>('searchPath'),
        };

        let s: string = '';
        if (this.buildOption?.targetOS) {
            s += "-T" + this.buildOption!.targetOS + " ";
        }
        if (this.buildOption?.targetCPU) {
            s += "-P" + this.buildOption!.targetCPU + " ";
        }
        if (this.buildOption?.outputFile) {
            let outfile=this.buildOption!.outputFile;
            if(outfile.startsWith("."))
            {
                outfile=path.join(vscode.workspace.workspaceFolders![0].uri.fsPath,outfile);
            }
            let dir=path.dirname(outfile);
            if(!fs.existsSync(dir)){
                try {
                    fs.mkdirSync(dir,{recursive:true});
                } catch (error) {
                    vscode.window.showErrorMessage("Can't create output directory.("+dir+")");
                }
               
            }
            s += "-o" + this.buildOption!.outputFile + " ";
        }
 
        globalOption?.searchPath?.forEach((e)=>{
            s+="-Fu"+e+" ";
        });
        globalOption?.libPath?.forEach((e)=>{
            s+="-Fl"+e+" ";
        });

        
        this.buildOption?.searchPath?.forEach((e)=>{
            s+="-Fu"+e+" ";
        });
        this.buildOption?.libPath?.forEach((e)=>{
            s+="-Fl"+e+" ";
        });


        if (this.buildOption?.unitOutputDir) {
            let dir=this.buildOption!.unitOutputDir;
            
            if(dir.startsWith("."))
            {
                dir=path.join(vscode.workspace.workspaceFolders![0].uri.fsPath,dir);
            }

            if(!fs.existsSync(dir)){
                try {
                    fs.mkdirSync(dir,{recursive:true});
                } catch (error) {
                    vscode.window.showErrorMessage("Can't create unit output directory.("+dir+")");
                }
               
            }
            s += "-FU" + this.buildOption!.unitOutputDir + " ";
        }

        if (this.buildOption?.optimizationLevel) {
            s += "-O" + this.buildOption!.optimizationLevel + " ";
        }

        if (this.buildOption?.syntaxMode) {
            s += "-M" + this.buildOption!.syntaxMode + " ";
        }
        globalOption?.customOptions?.forEach((e) => {
            s += e + " ";
        });
        this.buildOption?.customOptions?.forEach((e) => {
            s += e + " ";
        });

        return s;
    }

}
export class TaskInfo{
    ischanged:boolean=false;
    tasks:any;
}


export class InitializationOptions{
    //current work path
    public cwd: string |undefined;
    // Path to the main program file for resolving references
    // if not available the path of the current document will be used
    public program: string |undefined;
    // Path to SQLite3 database for symbols
    public symbolDatabase: string|undefined;
    // FPC compiler options (passed to Code Tools)
    public fpcOptions: Array<string>=[];
    // Maximum number of completion items to be returned
    // if the threshold is reached then CompletionList.isIncomplete = true
    public maximumCompletions:number=100;
    // Policy which determines how overloaded document symbols are displayed
    public overloadPolicy: number|undefined;
    // procedure completions with parameters are inserted as snippets
    public insertCompletionsAsSnippets: boolean|undefined;
    // procedure completions with parameters (non-snippet) insert
    // empty brackets (and insert as snippet)
    public insertCompletionProcedureBrackets: boolean|undefined;
    // workspaces folders will be added to unit paths (i.e. -Fu)
    public includeWorkspaceFoldersAsUnitPaths: boolean|undefined;
    // workspaces folders will be added to include paths (i.e. -Fi)
    public includeWorkspaceFoldersAsIncludePaths: boolean|undefined;
    // syntax will be checked when file opens or saves
    public checkSyntax: boolean|undefined;
    // syntax errors will be published as diagnostics
    public publishDiagnostics: boolean|undefined;
    // enable workspace symbols
    public workspaceSymbols: boolean|undefined;
    // enable document symbols
    public documentSymbols: boolean|undefined;
    // completions contain a minimal amount of extra information
    public minimalisticCompletions: boolean|undefined;
    // syntax errors as shown in the UI with ‘window/showMessage’
    public showSyntaxErrors: boolean|undefined;
    // ignores completion items like "begin" and "var" which may interfer with IDE snippets
    public ignoreTextCompletions: boolean|undefined;
    
	constructor (){
        let cfg=vscode.workspace.getConfiguration('fpctoolkit.lsp.initializationOptions');
        this.program=cfg.get<string>('program');
        this.maximumCompletions=cfg.get<number>('maximumCompletions',100);
        this.fpcOptions=cfg.get<Array<string>>("fpcOptions",[]);
        this.overloadPolicy=cfg.get<number>("overloadPolicy");
        this.insertCompletionsAsSnippets=cfg.get<boolean>('insertCompletionsAsSnippets');
        this.insertCompletionsAsSnippets=cfg.get<boolean>('insertCompletionsAsSnippets');
        this.includeWorkspaceFoldersAsIncludePaths=cfg.get<boolean>('insertCompletionsAsSnippets');
        this.includeWorkspaceFoldersAsUnitPaths=cfg.get<boolean>('includeWorkspaceFoldersAsUnitPaths');
        this.checkSyntax=cfg.get<boolean>('checkSyntax');
        this.publishDiagnostics=cfg.get<boolean>('publishDiagnostics');
        this.workspaceSymbols=cfg.get<boolean>('workspaceSymbols');
        this.documentSymbols=cfg.get<boolean>('documentSymbols');
        this.minimalisticCompletions=cfg.get<boolean>('minimalisticCompletions');
        this.showSyntaxErrors=cfg.get<boolean>('showSyntaxErrors');
        this.ignoreTextCompletions=cfg.get<boolean>('ignoreTextCompletions');
    }
    public updateByCompileOption(opt:CompileOption){
        this.cwd=opt.cwd;
        this.program=opt.file;
        let fpcOptions: Array<string> = this.fpcOptions;
        let newopt = opt.toOptionString().split(' ');
        newopt.forEach((s) => {
            if(s.startsWith('-Fi')||s.startsWith('-Fu')||s.startsWith('-d')||s.startsWith('-M')){
            //if (!s.startsWith('-v')) { //-v will raise error ,hide it 
                fpcOptions.push(s);
            }
        });

    }
}