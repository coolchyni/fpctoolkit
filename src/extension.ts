// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import {FpcProjectProvider, FpcItem} from './fpcProjectProvider';
import { FpcTaskProvider ,FpcTaskDefinition} from './fpcTaskProvider';
import { FpcCommandManager } from './fpcCommandManager';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	if (!vscode.workspace.workspaceFolders) {
		return;
	}
	
	const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;
	
	let commands=new FpcCommandManager(workspaceRoot);
	commands.registerAll(context);
	
	vscode.window.registerTreeDataProvider("FpcProjectExplorer",new FpcProjectProvider(workspaceRoot));

	context.subscriptions.push( vscode.tasks.registerTaskProvider(
		FpcTaskProvider.FpcTaskType,
		new FpcTaskProvider(workspaceRoot)
		)
	);

}

// this method is called when your extension is deactivated
export function deactivate() {}
