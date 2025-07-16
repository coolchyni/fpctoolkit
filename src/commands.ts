import * as vscode from 'vscode';
import { FpcItem, ProjectType } from './providers/project';
import * as fs from 'fs';
import * as fs2 from 'fs-extra';
import path = require('path');
import { BuildMode, FpcTask, FpcTaskDefinition, FpcTaskProvider, taskProvider } from './providers/task';
import { CompileOption } from './languageServer/options';
import { configuration } from './common/configuration'
import { type } from 'os';
import { client } from './extension';
import { TextEditor, TextEditorEdit } from 'vscode';


export class FpcCommandManager {
    constructor(private workspaceRoot: string) {

    }
    registerAll(context: vscode.ExtensionContext) {
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.build', this.ProjectBuild));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.rebuild', this.ProjectReBuild));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.clean', this.projectClean));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.opensetting', this.ProjectOpen));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.newproject', this.ProjectNew));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.add', this.ProjectAdd));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.setdefault', this.projectSetDefault));
        
        context.subscriptions.push(vscode.commands.registerTextEditorCommand('fpctoolkit.code.complete',this.CodeComplete));
    }
    ProjectAdd = async (node: FpcItem) => {
        if (node.level === 0) {
            // 如果是Lazarus项目，不允许添加新的构建配置，因为配置来自.lpi文件
            if (node.projectType === ProjectType.Lazarus) {
                vscode.window.showInformationMessage('Lazarus项目的构建配置由.lpi文件管理，无需手动添加。');
                return;
            }

            let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
            let inp = await vscode.window.showQuickPick(['debug', 'release', 'other...'], { canPickMany: false });
            if (!inp) {
                return;
            }
            let label: string | undefined;
            let customOption = '-dDEBUG';
            let isDebug=false;
            switch (inp) {
                case 'debug':
                    isDebug=true;
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
                "buildOption": {
                    "syntaxMode": "ObjFPC",
                    "unitOutputDir": "./out",
                    "customOptions": [
                        customOption
                    ]
                }
            };
            if(isDebug){
                v.buildOption.customOptions=[customOption,'-gw2'];
            }
            
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
    ProjectBuild = async (node: FpcItem) => {
        if (node.level === 0) {

        } else {
            vscode.tasks.fetchTasks({ type: 'fpc' }).then((e) => {
                e.forEach((task) => {
                    //vscode.window.showInformationMessage(task.name);
                    if (task.name === node.label) {
                        let newtask=taskProvider.taskMap.get(task.name);
                        if(newtask){
                            (newtask as FpcTask).BuildMode=BuildMode.normal;   
                        }
                        vscode.tasks.executeTask(task);

                        return;
                    }

                });
            });

        }

    };

    ProjectReBuild = async (node: FpcItem) => {

        if (node.level === 0) {

        } else {
            await this.projectClean(node);
            this.ProjectBuild(node);

            // vscode.tasks.fetchTasks({ type: 'fpc' }).then((e) => {

            //     for (const task of e) {
            //         if (task.name === node.label) {
            //             let newtask=taskProvider.taskMap.get(task.name);
            //             if(newtask){
            //                 (newtask as FpcTask).BuildMode=BuildMode.rebuild;   
            //             }
                        
            //             vscode.tasks.executeTask(task).then((e)=>{
            //                 console.log(e.task.name);
            //             });

            //             return;
            //         }
   
            //     }
            // });

        }

    };
    ProjectOpen = async (node?: FpcItem) => {

        let file = path.join(this.workspaceRoot, ".vscode", "tasks.json");
        let doc = await vscode.workspace.openTextDocument(file);
        let te = await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);

    };
    ProjectNew = async () => {

        let s = `program main;
{$mode objfpc}{$H+}
uses
  classes,sysutils;
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
    projectClean = async (node: FpcItem) => {

        let definition = taskProvider.GetTaskDefinition(node.label);

        let dir = definition?.buildOption?.unitOutputDir;
        if (!dir) { return; }
        if (!path.isAbsolute(dir)) {

            if (definition?.cwd) {
                let cur_dir=definition.cwd;
                if(cur_dir.startsWith('./') || cur_dir.startsWith('.\\')){
                    cur_dir=path.join(this.workspaceRoot,definition.cwd);
                }
                dir = path.join(cur_dir, dir);
            } else {
                dir = path.join(this.workspaceRoot, dir);
            }
        }

        let cleanExt = definition?.cleanExt;
        if (fs.existsSync(dir)) {
            try {
                let exts = ['.o', '.ppu', '.lfm', '.a', '.or', '.res','.rsj','.obj'];
                let isall = false;
                if (cleanExt) {
                    if ((<String>cleanExt).trim() == '*') {
                        isall = true;
                    }
                    let tmps = (<String>cleanExt).split(',');
                    for (const s of tmps) {
                        exts.push(s);
                    }
                }
                let files = fs.readdirSync(dir);
                for (let index = 0; index < files.length; index++) {
                    let file = files[index].toLowerCase();
                    let ext = path.extname(file);

                    if (isall || exts.includes(ext)) {
                        try {
                            fs2.removeSync(path.join(dir, file));
                        } catch {

                        }

                    }
                }

            } catch {

            }
        }
    };

    projectSetDefault = async (node: FpcItem) => {
        let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
        let tasks=config.tasks;
        for (const task of tasks) {
            if(task.label===node.label){
                if(typeof(task.group)==='object'){
                    task.group.isDefault=true;    
                }else{
                    task.group={kind:task.group,isDefault:true};
                }
                client.restart();
            }else{
                if(typeof(task.group)==='object'){
                    task.group.isDefault=undefined;
                }
            }
           

        }
        config.update(
            "tasks",
            tasks,
            vscode.ConfigurationTarget.WorkspaceFolder
        );
    }


    CodeComplete= async (textEditor: TextEditor, edit: TextEditorEdit) => {
        client.doCodeComplete(textEditor);
      
    }
}
