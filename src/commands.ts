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

        context.subscriptions.push(vscode.commands.registerTextEditorCommand('fpctoolkit.code.complete', this.CodeComplete));
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
            let isDebug = false;
            switch (inp) {
                case 'debug':
                    isDebug = true;
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
            if (isDebug) {
                v.buildOption.customOptions = [customOption, '-gw2'];
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
        // 只有子节点（构建配置）才能执行 Build 操作
        if (node.level === 0) {
            // 根节点（项目级别）不执行任何操作
            return;
        }

        // 处理 Lazarus 项目的子节点
        if (node.projectType === ProjectType.Lazarus) {
            // 确保文件路径正确
            if (!node.tasks || node.tasks.length === 0) {
                vscode.window.showErrorMessage('无效的任务定义');
                return;
            }

            const taskDef = node.tasks[0];
            if (!taskDef.file) {
                vscode.window.showErrorMessage('无效的文件路径');
                return;
            }

            // 确保文件存在
            const filePath = path.isAbsolute(taskDef.file)
                ? taskDef.file
                : path.join(this.workspaceRoot, taskDef.file);

            let finalFilePath = filePath;
            if (!fs.existsSync(filePath)) {
                // 尝试在工作区中查找文件
                const files = fs.readdirSync(this.workspaceRoot);
                const fileName = path.basename(taskDef.file);
                const matchingFiles = files.filter(f => f.toLowerCase() === fileName.toLowerCase());

                if (matchingFiles.length > 0) {
                    finalFilePath = path.join(this.workspaceRoot, matchingFiles[0]);
                    taskDef.file = matchingFiles[0];
                } else {
                    vscode.window.showErrorMessage(`找不到主程序文件: ${taskDef.file}`);
                    return;
                }
            }

            // 创建临时FPC任务
            const tempTask = taskProvider.getTask(
                node.label,
                taskDef.file,
                {
                    type: 'fpc',
                    file: taskDef.file,
                    buildOption: taskDef.buildOption || {}
                }
            );

            // 设置为普通构建模式
            let newtask = taskProvider.taskMap.get(tempTask.name);
            if (newtask) {
                (newtask as FpcTask).BuildMode = BuildMode.normal;
            }

            // 执行任务
            vscode.tasks.executeTask(tempTask);
        } else {
            // 处理 FPC 项目的子节点
            vscode.tasks.fetchTasks({ type: 'fpc' }).then((e) => {
                e.forEach((task) => {
                    if (task.name === node.label) {
                        let newtask = taskProvider.taskMap.get(task.name);
                        if (newtask) {
                            (newtask as FpcTask).BuildMode = BuildMode.normal;
                        }
                        vscode.tasks.executeTask(task);
                        return;
                    }
                });
            });
        }
    };

    ProjectReBuild = async (node: FpcItem) => {
        // 只有子节点（构建配置）才能执行 ReBuild 操作
        if (node.level === 0) {
            // 根节点（项目级别）不执行任何操作
            return;
        }

        // 处理 Lazarus 项目的子节点
        if (node.projectType === ProjectType.Lazarus) {
            // 确保文件路径正确
            if (!node.tasks || node.tasks.length === 0) {
                vscode.window.showErrorMessage('无效的任务定义');
                return;
            }

            const taskDef = node.tasks[0];
            if (!taskDef.file) {
                vscode.window.showErrorMessage('无效的文件路径');
                return;
            }

            // 确保文件存在
            const filePath = path.isAbsolute(taskDef.file)
                ? taskDef.file
                : path.join(this.workspaceRoot, taskDef.file);

            let finalFilePath = filePath;
            if (!fs.existsSync(filePath)) {
                // 尝试在工作区中查找文件
                const files = fs.readdirSync(this.workspaceRoot);
                const fileName = path.basename(taskDef.file);
                const matchingFiles = files.filter(f => f.toLowerCase() === fileName.toLowerCase());

                if (matchingFiles.length > 0) {
                    finalFilePath = path.join(this.workspaceRoot, matchingFiles[0]);
                    taskDef.file = matchingFiles[0];
                } else {
                    vscode.window.showErrorMessage(`找不到主程序文件: ${taskDef.file}`);
                    return;
                }
            }

            // 确保构建选项包含强制重新构建标志
            if (!taskDef.buildOption) {
                taskDef.buildOption = {};
            }
            taskDef.buildOption.forceRebuild = true;

            // 创建临时FPC任务
            const tempTask = taskProvider.getTask(
                node.label,
                taskDef.file,
                {
                    type: 'fpc',
                    file: taskDef.file,
                    buildOption: taskDef.buildOption
                }
            );

            // 设置为重新构建模式
            let newtask = taskProvider.taskMap.get(tempTask.name);
            if (newtask) {
                (newtask as FpcTask).BuildMode = BuildMode.rebuild;
            }

            // 执行任务
            vscode.tasks.executeTask(tempTask);
        } else {
            // 对于 FPC 项目的子节点，先清理然后构建
            await this.projectClean(node);
            this.ProjectBuild(node);
        }
    };
    ProjectOpen = async (node?: FpcItem) => {
        // 如果提供了节点，并且是 Lazarus 项目，则打开 .lpi 文件
        if (node && node.projectType === ProjectType.Lazarus) {
            const lpiFile = path.join(this.workspaceRoot, node.file);
            if (fs.existsSync(lpiFile)) {
                const doc = await vscode.workspace.openTextDocument(lpiFile);
                await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
                return;
            }
        }

        // 默认情况下打开 tasks.json
        const file = path.join(this.workspaceRoot, ".vscode", "tasks.json");
        if (fs.existsSync(file)) {
            const doc = await vscode.workspace.openTextDocument(file);
            await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
        } else {
            vscode.window.showErrorMessage("找不到任务配置文件");
        }
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
        let definition;
        let dir;

        // 处理 Lazarus 项目
        if (node.projectType === ProjectType.Lazarus && node.tasks && node.tasks.length > 0) {
            // 从 Lazarus 项目任务中获取定义
            definition = node.tasks[0];
            // Lazarus 项目通常使用 ./lib 作为输出目录
            dir = definition?.buildOption?.unitOutputDir || './lib';
        } else {
            // 处理 FPC 项目
            definition = taskProvider.GetTaskDefinition(node.label);
            dir = definition?.buildOption?.unitOutputDir;
        }

        if (!dir) { return; }

        if (!path.isAbsolute(dir)) {
            if (definition?.cwd) {
                let cur_dir = definition.cwd;
                if (cur_dir.startsWith('./') || cur_dir.startsWith('.\\')) {
                    cur_dir = path.join(this.workspaceRoot, definition.cwd);
                }
                dir = path.join(cur_dir, dir);
            } else {
                dir = path.join(this.workspaceRoot, dir);
            }
        }

        // 如果是 Lazarus 项目，还需要检查项目目录下的编译输出
        if (node.projectType === ProjectType.Lazarus) {
            const lpiPath = path.join(this.workspaceRoot, node.file);
            const projectDir = path.dirname(lpiPath);

            // 检查项目目录是否存在
            if (fs.existsSync(projectDir)) {
                // 清理项目目录中的编译输出文件
                this.cleanDirectory(projectDir);
            }
        }

        let cleanExt = definition?.cleanExt;
        if (fs.existsSync(dir)) {
            this.cleanDirectory(dir, cleanExt);
        }
    };

    // 辅助方法：清理目录中的编译输出文件
    private cleanDirectory(dir: string, cleanExt?: string) {
        try {
            let exts = ['.o', '.ppu', '.lfm', '.a', '.or', '.res', '.rsj', '.obj'];
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
                        // 忽略删除失败的情况
                    }
                }
            }
        } catch {
            // 忽略目录处理失败的情况
        }
    }

    projectSetDefault = async (node: FpcItem) => {
        // 如果是 Lazarus 项目，我们需要特殊处理
        if (node.projectType === ProjectType.Lazarus) {
            // 对于 Lazarus 项目，我们可以在内存中标记它为默认项目
            // 但实际上 Lazarus 项目的配置是由 .lpi 文件管理的，不是 tasks.json

            // 通知用户
            vscode.window.showInformationMessage(`已将 ${node.label} 设置为默认项目`);

            // 重启客户端以应用更改
            client.restart();
            return;
        }

        // 对于 FPC 项目，继续使用原来的逻辑
        let config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
        let tasks = config.tasks;

        if (!tasks) {
            vscode.window.showErrorMessage('没有找到任务配置');
            return;
        }

        for (const task of tasks) {
            if (task.label === node.label) {
                if (typeof (task.group) === 'object') {
                    task.group.isDefault = true;
                } else {
                    task.group = { kind: task.group, isDefault: true };
                }
            } else {
                if (typeof (task.group) === 'object') {
                    task.group.isDefault = undefined;
                }
            }
        }

        // 更新配置
        config.update(
            "tasks",
            tasks,
            vscode.ConfigurationTarget.WorkspaceFolder
        );

        // 重启客户端以应用更改
        client.restart();

        vscode.window.showInformationMessage(`已将 ${node.label} 设置为默认项目`);
    }


    CodeComplete = async (textEditor: TextEditor, edit: TextEditorEdit) => {
        client.doCodeComplete(textEditor);

    }
}
