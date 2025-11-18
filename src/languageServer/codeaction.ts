/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/
// sample
// https://github.com/microsoft/vscode-extension-samples/blob/main/code-actions-sample/src/extension.ts

import { match } from 'assert';
import { disconnect } from 'process';
import * as vscode from 'vscode';
import { diagCollection } from '../providers/task';
import { number } from 'zod';

const COMMAND_UNUSED = 'fpctoolkit.code-actions.remove_unused_variable';

export function activate(context: vscode.ExtensionContext) {

	context.subscriptions.push(
		vscode.languages.registerCodeActionsProvider('objectpascal', new FpcCodeAction(), {
			providedCodeActionKinds: FpcCodeAction.providedCodeActionKinds
		})
	);

	context.subscriptions.push(
		vscode.commands.registerCommand(COMMAND_UNUSED, (document:vscode.TextDocument,diag:vscode.Diagnostic,variable:string) => {
            let edit=new vscode.WorkspaceEdit();
            let line=document.lineAt(diag.range.start.line);
            let linetext=line.text.trim();
            var has_var=false;
            if(linetext.substring(0,4).toLocaleLowerCase()=='var ')
            {
                linetext=linetext.substring(4,linetext.length)
                has_var=true;
            }
            let isdelete=false;
            let ret=linetext.split(/,|:/);
            if(ret.length<=2){
                if(has_var){
                    edit.replace(document.uri,line.range,"var");
                }else{
                    edit.delete(document.uri,line.rangeIncludingLineBreak);
                    isdelete=true;
                }
            }else{
                ret=ret.map(v=>v.trim()).filter(v=>v!=variable);
                linetext=has_var?'var ':'  '+ret.slice(0,-1).join(', ')+': '+ret[ret.length-1];
                edit.replace(document.uri,line.range,linetext);
            }
        
            let diags= diagCollection.get(document.uri);
            if(!diags){
                vscode.workspace.applyEdit(edit);
                return;
            }
            if(!isdelete){
                //delete fixed diag

                var newdiags=[];
                for (const item of diags) {
                    if(item===diag)
                    {
                        continue;
                    }
    
                    //chagne diag pos use workspace.onDidChangeTextDocument
                    // if(isdelete && item.range.start.line>=line.range.start.line){
                    //     item.range=new vscode.Range(item.range.start.line-1,item.range.start.character,item.range.end.line-1,item.range.end.character)
                    // }
                    newdiags.push(item)
                }
                diagCollection.set(document.uri, newdiags);
            }
     
           
            vscode.workspace.applyEdit(edit);

        })
	);
}



/**
 * Provides code actions corresponding to diagnostic problems.
 */
export class FpcCodeAction implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix,
        vscode.CodeActionKind.Source,
        vscode.CodeActionKind.Refactor
	];

	provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.CodeAction[] {
		// for each diagnostic entry that has the matching `code`, create a code action command
	    let actions = context.diagnostics
			.filter(diagnostic =>{
                return diagnostic.code === 5025;
            } )
			.map(diagnostic => this.createCommandCodeAction(document,diagnostic));
        
        if(context.triggerKind===vscode.CodeActionTriggerKind.Invoke){
            // CodeComplete
            const action = new vscode.CodeAction(
                "Complete Code",
                vscode.CodeActionKind.Source
            );

            action.command = {
                title: "Complete Code",
                command: "pasls.completeCode",
                arguments: [document.uri.toString(), range.start]
            };
            actions.push(action);

            const action_removeEmptyMethods = new vscode.CodeAction(
                "Remove Empty Methods",
                vscode.CodeActionKind.Refactor
            );
            action_removeEmptyMethods.command = {
                title: "Remove Empty Methods",
                command: "pasls.removeEmptyMethods",
                arguments: [document.uri.toString(), range.start]
            };
            actions.push(action_removeEmptyMethods);

            const action_removeUnusedUnits = new vscode.CodeAction(
                "Remove Unused Units",
                vscode.CodeActionKind.Refactor
            );
            action_removeUnusedUnits.command = {
                title: "Remove Unused Units",
                command: "pasls.removeUnusedUnits",
                arguments: [document.uri.toString(), range.start]
            };
            actions.push(action_removeUnusedUnits);

            let nrange = new vscode.Range(range.start.line, 0, range.end.line, 9999);
            let s = document.getText(nrange);
            if(s.indexOf(':=')>0){
                const action_invertAssign = new vscode.CodeAction(
                    "Invert Assignment",
                    vscode.CodeActionKind.Refactor
                );
                action_invertAssign.command = {
                    title: "Invert Assignment",
                    command: "pasls.invertAssignment",
                    arguments: [document.uri.toString(), nrange.start,nrange.end]
                };
                actions.push(action_invertAssign);
            }
        }
        return actions;
	}

	private createCommandCodeAction(document: vscode.TextDocument,diagnostic: vscode.Diagnostic): vscode.CodeAction {
        let matchs=diagnostic.message.match(/Local variable "(.*?)".*?not used/)!;
     
        let variable=matchs[1];

		const fix = new vscode.CodeAction('Remove variable `'+variable+'`', vscode.CodeActionKind.QuickFix);
		fix.command = { command: COMMAND_UNUSED, title: 'Remove unused variable.', tooltip: 'Remove unused variable.' };
        fix.command.arguments=[document, diagnostic,variable];
		fix.diagnostics = [diagnostic];
		//fix.isPreferred = true;
		return fix;
	}
}