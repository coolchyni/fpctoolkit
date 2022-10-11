// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { FpcItem, FpcProjectProvider } from './providers/project';
import { diagCollection, FpcTaskProvider,taskProvider } from './providers/task';
import { FpcCommandManager } from './commands';
import * as util from './common/util';
import {TLangClient} from './languageServer/client';
import { configuration } from './common/configuration';
import { JediFormatter } from './formatter';
import { format } from 'path';
import * as MyCodeAction from  './languageServer/codeaction';
export let client:TLangClient;
export let formatter:JediFormatter;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: vscode.ExtensionContext) {

	if (!vscode.workspace.workspaceFolders) {
		return;
	}


	vscode.window.onDidChangeVisibleTextEditors(onDidChangeVisibleTextEditors);
	vscode.workspace.onDidChangeTextDocument(onDidChangeTextDocument);
	util.setExtensionContext(context);
	const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;


	let commands = new FpcCommandManager(workspaceRoot);
	commands.registerAll(context);

	formatter=new JediFormatter();
	formatter.doInit();
	
	let ProjectProvider= new FpcProjectProvider(workspaceRoot,context);
	vscode.window.registerTreeDataProvider("FpcProjectExplorer", ProjectProvider);

	//taskProvider=new FpcTaskProvider(workspaceRoot);
	context.subscriptions.push(vscode.tasks.registerTaskProvider(
		FpcTaskProvider.FpcTaskType,
		taskProvider
	)
	);

	

	MyCodeAction.activate(context);

	client=new TLangClient(ProjectProvider);
	await client.doInit();
	client.start();


}

function onDidChangeTextDocument(e:vscode.TextDocumentChangeEvent){
	if(e.contentChanges.length>0){
		if(!diagCollection.has(e.document.uri)){ return ;}
		
		
		for (const change of e.contentChanges) {
			let newline=(change.text.match(/\n/g) || '').length+1;
			if(change.range.isSingleLine && (newline<2)){
				continue;
			}
			let diags= diagCollection.get(e.document.uri);
			if(!diags){return;}

			let oldline=change.range.end.line-change.range.start.line+1;
			
			let lines_change=newline-oldline;
			let newdiags=[];
			for (const diag of diags) {
				if(change.range.contains(diag.range)){//remove it if contains
					continue;
				}
			
				if(diag.range.start.line>=change.range.start.line){
					diag.range=new vscode.Range(diag.range.start.line+lines_change,diag.range.start.character,diag.range.end.line+lines_change,diag.range.end.character)
				}
				newdiags.push(diag)
			}
			diagCollection.set(e.document.uri, newdiags);
		}
		
		
		
	}
	
}
function onDidChangeVisibleTextEditors(editors: readonly vscode.TextEditor[]): void {
    // Process delayed didOpen for any visible editors we haven't seen before
    editors.forEach(editor => {
        if ((editor.document.uri.scheme === "file") && (editor.document.languageId === "objectpascal" || editor.document.languageId === "pascal" )) {
			editor.options.tabSize=configuration.get<number>('format.tabsize',2);
			client.onDidChangeVisibleTextEditor(editor);
        }
    });
}
// this method is called when your extension is deactivated
export function deactivate() {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
