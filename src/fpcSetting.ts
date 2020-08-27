import * as vscode from 'vscode';
import *  as fs from 'fs';
import * as path from 'path';
export class CompileOption {
    /**
     * Compile Option
     */
    public type:string="fpc";
    //public problemMatcher="$fpc";
    public presentation={
        showReuseMessage: false,
		clear: true,
        revealProblems: "onProblem"
    };
            
    public buildOption?:{
         targetOS?: string,
         targetCPU?: string,
         customOptions?:string [],    
         libPath?: [],
         outputFile?: string,
         unitOutputDir?: string,
         optimizationLevel?: number,
         searchPath?: [],
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
export class FpcSetting {
    public compileOptions: CompileOption[] = [];

    constructor(private taskinfo: TaskInfo , public readonly file: string) {
        this.bindFile();

    }
    bindFile() {

        // retrieve values
       
        if (this.taskinfo.tasks === undefined) {
            this.taskinfo.tasks = [];
        }
        this.taskinfo.tasks.forEach((e: any) => {
            if ((e.type==="fpc") && (e.file === this.file)) {
                let opt = new CompileOption(e.label,e.file);
                opt.buildOption=e.buildOption;
                this.compileOptions.push(opt);
            }
        });
        if(this.compileOptions.length===0){
            let label="fpc:build:"+this.file.substr(0,this.file.length-4);
            let opt = new CompileOption(label,this.file);
            this.compileOptions[0]=opt;
            this.taskinfo.tasks.push(opt);
            this.taskinfo.ischanged=true;
        }

        return ;

       
    }
}