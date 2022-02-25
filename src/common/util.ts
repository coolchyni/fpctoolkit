import path = require("path");
import * as vscode from 'vscode';
/**
 * @File   : util.ts
 * @Author :  (coolchyni)
 * @Link   : 
 * @Date   : 2/13/2022, 11:32:21 AM
 * some function copy form https://github.com/microsoft/vscode-cpptools/blob/main/Extension/src/common.ts
 */

 export let extensionPath: string;
 export let extensionContext: vscode.ExtensionContext | undefined;
 export function setExtensionContext(context: vscode.ExtensionContext): void {
     extensionContext = context;
     extensionPath = extensionContext.extensionPath;
 }
 export function setExtensionPath(path: string): void {
     extensionPath = path;
 }

 export function getExtensionFilePath(extensionfile: string): string {
    return path.resolve(extensionPath, extensionfile);
}
export function isVsCodeInsiders(): boolean {
    return extensionPath.includes(".vscode-insiders") ||
        extensionPath.includes(".vscode-server-insiders") ||
        extensionPath.includes(".vscode-exploration") ||
        extensionPath.includes(".vscode-server-exploration");
}