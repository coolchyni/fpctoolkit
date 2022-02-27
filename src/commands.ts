import * as vscode from 'vscode';
import { FpcItem } from './providers/project';
import * as fs from 'fs';
import * as fs2 from 'fs-extra';
import path = require('path');

export class FpcCommandManager {
    constructor(private workspaceRoot: string) {

    }
    registerAll(context: vscode.ExtensionContext) {
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.build', this.ProjectBuild));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.clean', this.projectClean));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.opensetting', this.ProjectOpen));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.newproject', this.ProjectNew));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.add', this.ProjectAdd));
    }
    ProjectAdd=async (node: FpcItem) =>{
        if (node.level === 0) {
            let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
            let inp = await vscode.window.showQuickPick(['debug', 'release', 'other...'], { canPickMany: false });
            if (!inp) {
                return;
            }
            let label: string | undefined;
            let customOption = '-dDEBUG';
            switch (inp) {
                case 'debug':
                    label = 'debug';
                    break;
                case 'release':
                    label = 'release';
                    customOption = '-dRELEASE';
                    break;

                default:
                    label = await vscode.window.showInputBox({ prompt: 'Input build label:' });

                    break;
            }
            if (!label) {
                return;
            }
            let v = {
                "label": label,
                "file": node.label,
                "type": "fpc",
                "presentation": {
                    "showReuseMessage": false,
                    "clear": true,
                    "revealProblems": "onProblem"
                },
                "buildOption": {
                    "unitOutputDir": "./out",
                    "customOptions": [
                        customOption
                    ]
                }
            };

            let tasks = config.tasks;
            if (tasks) {
                tasks.push(v);
            } else {
                tasks = [v];
            }
            config.update(
                "tasks",
                tasks,
                vscode.ConfigurationTarget.WorkspaceFolder
            );
        }

    };
    ProjectBuild=async (node: FpcItem) =>{
        if (node.level === 0) {

        } else {
            vscode.tasks.fetchTasks({ type: 'fpc' }).then((e) => {
                e.forEach((task) => {
                    //vscode.window.showInformationMessage(task.name);
                    if (task.name === node.label) {
                        vscode.tasks.executeTask(task);
                        return;
                    }

                });
            });

        }

    };
    ProjectOpen = async (node?: FpcItem) => {

        let file = path.join(this.workspaceRoot, ".vscode", "tasks.json");
        let doc = await vscode.workspace.openTextDocument(file);
        let te = await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);

    };
    ProjectNew =async () => {

        let s = `program main;
begin 
   
end.`;

        let file = path.join(this.workspaceRoot, "main.lpr");


        fs.writeFile(file, s, () => {
            let f = vscode.workspace.openTextDocument(file);
            f.then((doc) => {
                vscode.window.showTextDocument(doc, vscode.ViewColumn.One)
                    .then((e: vscode.TextEditor) => {
                        let pos = new vscode.Position(2, 4);
                        e.selection = new vscode.Selection(pos, pos);
                    });

            });

        });

        let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));

        let v = {
            "label": "debug",
            "file": "main.lpr",
            "type": "fpc",
            "presentation": {
                "showReuseMessage": false,
                "clear": true,
                "revealProblems": "onProblem"
            },
            "buildOption": {
                "unitOutputDir": "./out",
                "customOptions": [
                    "-dDEBUG"
                ]
            }
        };
        let tasks = config.tasks;
        if (tasks) {
            tasks.push(v);
        } else {
            tasks = [v];
        }
        config.update(
            "tasks",
            tasks,
            vscode.ConfigurationTarget.WorkspaceFolder
        );

    };
    projectClean=async (node: FpcItem) =>{
        if (!node.tasks) { return; }
        let dir = (node.tasks[0]).buildOption?.unitOutputDir;
        if (!dir) { return; }
        if(!path.isAbsolute(dir)){
            if(node.tasks[0].cwd){
                dir = path.join(node.tasks[0].cwd, dir);
            }else{
                dir = path.join(this.workspaceRoot, dir);
            }
        }
       
        let cleanExt=node.tasks[0].cleanExt;
        if (fs.existsSync(dir)) {
            try {
                let exts=['.o','.ppu'];
                let isall=false;
                if(cleanExt){
                    if((<String>cleanExt).trim()=='*'){
                        isall=true;
                    }
                    let tmps=(<String>cleanExt).split(',');
                    for (const s of tmps) {
                        exts.push(s);
                    }
                }
                let files = fs.readdirSync(dir);
				for (let index = 0; index < files.length; index++) {
					let file = files[index].toLowerCase();
                    let ext=path.extname(file);
                        
                    if(isall || exts.includes(ext)){
                        try{
                            fs2.removeSync(path.join(dir,file));
                        }catch{

                        }
                        
                    }
                }
	
            } catch{

            }
        };

    };
}
