/*---------------------------------------------------------
 * Copyright (C) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------*/
// sample
// https://github.com/microsoft/vscode-extension-samples/blob/main/code-actions-sample/src/extension.ts

import { match } from 'assert';
import { disconnect } from 'process';
import * as vscode from 'vscode';
import { diagCollection } from '../providers/task';

const COMMAND_UNUSED = 'fpctoolkit.code-actions.remove_unused_variable';

export function activate(context: vscode.ExtensionContext) {
	// context.subscriptions.push(
	// 	vscode.languages.registerCodeActionsProvider('objectpascal', new Emojizer(), {
	// 		providedCodeActionKinds: Emojizer.providedCodeActionKinds
	// 	}));

	// const emojiDiagnostics = vscode.languages.createDiagnosticCollection("emoji");
	// context.subscriptions.push(emojiDiagnostics);

	

	context.subscriptions.push(
		vscode.languages.registerCodeActionsProvider('objectpascal', new Emojinfo(), {
			providedCodeActionKinds: Emojinfo.providedCodeActionKinds
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
 * Provides code actions for converting :) to a smiley emoji.
 */
// export class Emojizer implements vscode.CodeActionProvider {

// 	public static readonly providedCodeActionKinds = [
// 		vscode.CodeActionKind.QuickFix
// 	];

// 	public provideCodeActions(document: vscode.TextDocument, range: vscode.Range): vscode.CodeAction[] | undefined {
// 		if (!this.isAtStartOfSmiley(document, range)) {
// 			return;
// 		}

// 		const replaceWithSmileyCatFix = this.createFix(document, range, '😺');

// 		const replaceWithSmileyFix = this.createFix(document, range, '😀');
// 		// Marking a single fix as `preferred` means that users can apply it with a
// 		// single keyboard shortcut using the `Auto Fix` command.
// 		replaceWithSmileyFix.isPreferred = true;

// 		const replaceWithSmileyHankyFix = this.createFix(document, range, '💩');

// 		const commandAction = this.createCommand();

// 		return [
// 			replaceWithSmileyCatFix,
// 			replaceWithSmileyFix,
// 			replaceWithSmileyHankyFix,
// 			commandAction
// 		];
// 	}

// 	private isAtStartOfSmiley(document: vscode.TextDocument, range: vscode.Range) {
// 		const start = range.start;
// 		const line = document.lineAt(start.line);
// 		return line.text[start.character] === ':' && line.text[start.character + 1] === ')';
// 	}

// 	private createFix(document: vscode.TextDocument, range: vscode.Range, emoji: string): vscode.CodeAction {
// 		const fix = new vscode.CodeAction(`Convert to ${emoji}`, vscode.CodeActionKind.QuickFix);
// 		fix.edit = new vscode.WorkspaceEdit();
// 		fix.edit.replace(document.uri, new vscode.Range(range.start, range.start.translate(0, 2)), emoji);
// 		return fix;
// 	}

// 	private createCommand(): vscode.CodeAction {
// 		const action = new vscode.CodeAction('Learn more...', vscode.CodeActionKind.Empty);
// 		action.command = { command: COMMAND, title: 'Learn more about emojis', tooltip: 'This will open the unicode emoji page.' };
// 		return action;
// 	}
// }

/**
 * Provides code actions corresponding to diagnostic problems.
 */
export class Emojinfo implements vscode.CodeActionProvider {

	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix
	];

	provideCodeActions(document: vscode.TextDocument, range: vscode.Range | vscode.Selection, context: vscode.CodeActionContext, token: vscode.CancellationToken): vscode.CodeAction[] {
		// for each diagnostic entry that has the matching `code`, create a code action command
	    return context.diagnostics
			.filter(diagnostic =>{
                return diagnostic.code === "variable-not-used";
            } )
			.map(diagnostic => this.createCommandCodeAction(document,diagnostic));
	}

	private createCommandCodeAction(document: vscode.TextDocument,diagnostic: vscode.Diagnostic): vscode.CodeAction {
        let matchs=diagnostic.message.match(/ Local variable "(.*?)".*?(?:not|never) used/)!;
     
        let variable=matchs[1];

		const fix = new vscode.CodeAction('Remove variable `'+variable+'`', vscode.CodeActionKind.QuickFix);
		fix.command = { command: COMMAND_UNUSED, title: 'Remove unused variable.', tooltip: 'Remove unused variable.' };
        fix.command.arguments=[document, diagnostic,variable];
		fix.diagnostics = [diagnostic];
		//fix.isPreferred = true;
		return fix;
	}
}