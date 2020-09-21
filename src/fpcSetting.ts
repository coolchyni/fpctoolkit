import * as vscode from 'vscode';
import *  as fs from 'fs';
import * as path from 'path';
export class CompileOption {
    /**
     * Compile Option
     */
    public type:string="fpc";

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
        public file: string
    ) {
        this.buildOption={
            unitOutputDir:"./out",
            customOptions:[
                "-dDEBUG"
            ]
        };
       
    }
    
    toOptionString() {
        let fpccfg = vscode.workspace.getConfiguration("fpctoolkit");
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
