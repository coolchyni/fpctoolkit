// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { FpcProjectProvider, FpcItem } from './providers/project';
import { FpcTaskProvider, FpcTaskDefinition } from './providers/task';
import { FpcCommandManager } from './commands';
import { CompileOption } from './languageServer/options';
import { workspace, ExtensionContext, WorkspaceFolder, Uri } from 'vscode';
import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	NotificationType,
	ServerOptions,
	ShowMessageNotification,
	ShowMessageParams,
	StreamInfo,
	TransportKind
} from 'vscode-languageclient';
import * as net from 'net';
import * as util from './common/util';
import { log } from 'console';
import { TextDecoder } from 'util';
import path = require('path');
import { resolve } from 'path';
import {TLangClient} from './languageServer/client';
import { configuration } from './common/configuration';

let client:TLangClient;

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	if (!vscode.workspace.workspaceFolders) {
		return;
	}
	vscode.window.onDidChangeVisibleTextEditors(onDidChangeVisibleTextEditors);
	util.setExtensionContext(context);
	const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.fsPath;

	let commands = new FpcCommandManager(workspaceRoot);
	commands.registerAll(context);

	let ProjectProvider= new FpcProjectProvider(workspaceRoot,context);
	vscode.window.registerTreeDataProvider("FpcProjectExplorer", ProjectProvider);

	let taskProvider=new FpcTaskProvider(workspaceRoot);
	context.subscriptions.push(vscode.tasks.registerTaskProvider(
		FpcTaskProvider.FpcTaskType,
		taskProvider
	)
	);
	
	client=new TLangClient(ProjectProvider);
	client.doInit();
	client.start();

	

}
function onDidChangeVisibleTextEditors(editors: vscode.TextEditor[]): void {
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
