import * as vscode from 'vscode';
import { FpcItem } from './fpcProjectProvider';
import { FpcTaskProvider } from './fpcTaskProvider';
import * as fs from 'fs';
import path = require('path');

export class FpcCommandManager {
    constructor(private workspaceRoot: string) {

    }
    registerAll(context: vscode.ExtensionContext){
        context.subscriptions.push( vscode.commands.registerCommand('fpctoolkit.project.build',this.ProjectBuild));
        context.subscriptions.push( vscode.commands.registerCommand('fpctoolkit.project.opensetting',this.ProjectOpen));
        context.subscriptions.push(vscode.commands.registerCommand('fpctoolkit.project.newproject',this.ProjectNew));
     }
    ProjectBuild = (node: FpcItem) => {
        if(node.level===0){
           
        }else{

            vscode.tasks.fetchTasks({type:'fpc'}).then((e)=>{
                e.forEach((task)=>{
                    //vscode.window.showInformationMessage(task.name);
                    if(task.name===node.label){
                        vscode.tasks.executeTask(task);
                    }

                });
            });
          
        }
        
    };
    ProjectOpen = () => {
        
        let file=path.join(this.workspaceRoot,".vscode","tasks.json");
        let f=vscode.workspace.openTextDocument(file);
        f.then((doc)=>{
            vscode.window.showTextDocument(doc,vscode.ViewColumn.One);
        });
        
    };
    ProjectNew = () => {
      
        let s=`program main;
begin 
   
end.`;

        let file=path.join(this.workspaceRoot,"main.lpr");
        
       
        fs.writeFile(file,s,()=>{
            let f=vscode.workspace.openTextDocument(file);
            f.then((doc)=>{
                vscode.window.showTextDocument(doc,vscode.ViewColumn.One)
                .then((e:vscode.TextEditor)=>{
                    let pos=new vscode.Position(2,4);
                    e.selection=new vscode.Selection(pos,pos);
                });
                
            });    

        });
        
    };

}
