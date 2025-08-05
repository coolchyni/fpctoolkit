import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import * as fs2 from 'fs-extra';

export interface ProjectTemplate {
    name: string;
    description: string;
    files: TemplateFile[];
    tasks?: any[];
}

export interface TemplateFile {
    path: string;
    content: string;
    cursorPosition?: { line: number; column: number };
}

export class ProjectTemplateManager {
    private static readonly DEFAULT_TEMPLATE_DIR = 'templates';
    private workspaceRoot: string;

    constructor(workspaceRoot: string) {
        this.workspaceRoot = workspaceRoot;
    }

    /**
     * 获取模板目录路径
     */
    private getTemplateDirectories(): string[] {
        const config = vscode.workspace.getConfiguration('fpctoolkit');
        const customTemplateDirs = config.get<string[]>('project.templateDirectories', []);
        
        const directories: string[] = [];
        
        // 添加自定义模板目录
        for (const dir of customTemplateDirs) {
            if (path.isAbsolute(dir)) {
                directories.push(dir);
            } else {
                directories.push(path.join(this.workspaceRoot, dir));
            }
        }
        
        // 添加默认模板目录
        const defaultDir = path.join(this.workspaceRoot, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        if (!directories.includes(defaultDir)) {
            directories.push(defaultDir);
        }
        
        return directories;
    }

    /**
     * 获取所有可用的项目模板
     */
    async getAvailableTemplates(): Promise<ProjectTemplate[]> {
        const templates: ProjectTemplate[] = [];
        
        // 添加内置的默认模板
        templates.push(this.getDefaultFpcTemplate());
        
        // 扫描模板目录
        const templateDirs = this.getTemplateDirectories();
        
        for (const templateDir of templateDirs) {
            if (fs.existsSync(templateDir)) {
                const templateFolders = fs.readdirSync(templateDir, { withFileTypes: true })
                    .filter(dirent => dirent.isDirectory())
                    .map(dirent => dirent.name);
                
                for (const templateFolder of templateFolders) {
                    const templatePath = path.join(templateDir, templateFolder);
                    const template = await this.loadTemplate(templatePath);
                    if (template) {
                        templates.push(template);
                    }
                }
            }
        }
        
        return templates;
    }

    /**
     * 获取默认的FPC项目模板
     */
    private getDefaultFpcTemplate(): ProjectTemplate {
        return {
            name: 'New FPC Project',
            description: 'Create a standard FPC project with main.lpr',
            files: [
                {
                    path: 'main.lpr',
                    content: `program main;
{$mode objfpc}{$H+}
uses
  classes,sysutils;
begin 
   
end.`,
                    cursorPosition: { line: 5, column: 4 }
                }
            ],
            tasks: [
                {
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
                }
            ]
        };
    }

    /**
     * 从目录加载模板
     */
    private async loadTemplate(templatePath: string): Promise<ProjectTemplate | null> {
        try {
            const configPath = path.join(templatePath, 'template.json');
            if (!fs.existsSync(configPath)) {
                return null;
            }

            const configContent = fs.readFileSync(configPath, 'utf8');
            const config = JSON.parse(configContent);

            const template: ProjectTemplate = {
                name: config.name || path.basename(templatePath),
                description: config.description || '',
                files: [],
                tasks: config.tasks
            };

            // 加载模板文件
            if (config.files && Array.isArray(config.files)) {
                for (const fileConfig of config.files) {
                    const filePath = path.join(templatePath, fileConfig.source);
                    if (fs.existsSync(filePath)) {
                        let content = fs.readFileSync(filePath, 'utf8');
                        
                        // 支持变量替换
                        content = this.processTemplateVariables(content);
                        
                        template.files.push({
                            path: fileConfig.target || fileConfig.source,
                            content: content,
                            cursorPosition: fileConfig.cursorPosition
                        });
                    }
                }
            }

            return template;
        } catch (error) {
            console.error(`Failed to load template from ${templatePath}:`, error);
            return null;
        }
    }

    /**
     * 处理模板变量
     */
    private processTemplateVariables(content: string): string {
        const now = new Date();
        const variables: { [key: string]: string } = {
            '{{DATE}}': now.toLocaleDateString(),
            '{{TIME}}': now.toLocaleTimeString(),
            '{{YEAR}}': now.getFullYear().toString(),
            '{{MONTH}}': (now.getMonth() + 1).toString().padStart(2, '0'),
            '{{DAY}}': now.getDate().toString().padStart(2, '0'),
            '{{USER}}': process.env.USER || process.env.USERNAME || 'User'
        };

        let result = content;
        for (const [variable, value] of Object.entries(variables)) {
            result = result.replace(new RegExp(variable, 'g'), value);
        }

        return result;
    }

    /**
     * 创建项目从模板
     */
    async createProjectFromTemplate(template: ProjectTemplate): Promise<void> {
        try {
            // 创建文件
            for (const file of template.files) {
                const filePath = path.join(this.workspaceRoot, file.path);
                const fileDir = path.dirname(filePath);
                
                // 确保目录存在
                if (!fs.existsSync(fileDir)) {
                    fs2.ensureDirSync(fileDir);
                }
                
                // 写入文件
                fs.writeFileSync(filePath, file.content);
            }

            // 添加任务配置
            if (template.tasks && template.tasks.length > 0) {
                await this.addTasksToWorkspace(template.tasks);
            }

            // 打开主文件
            const mainFile = template.files.find(f => 
                f.cursorPosition || 
                f.path.endsWith('.lpr') || 
                f.path.endsWith('.dpr') || 
                f.path.endsWith('.pas')
            );

            if (mainFile) {
                const filePath = path.join(this.workspaceRoot, mainFile.path);
                const doc = await vscode.workspace.openTextDocument(filePath);
                const editor = await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
                
                if (mainFile.cursorPosition) {
                    const pos = new vscode.Position(mainFile.cursorPosition.line, mainFile.cursorPosition.column);
                    editor.selection = new vscode.Selection(pos, pos);
                }
            }

            vscode.window.showInformationMessage(`Project created from template: ${template.name}`);
        } catch (error) {
            vscode.window.showErrorMessage(`Failed to create project from template: ${error}`);
        }
    }

    /**
     * 添加任务到工作区
     */
    private async addTasksToWorkspace(tasks: any[]): Promise<void> {
        const config = vscode.workspace.getConfiguration('tasks', vscode.Uri.file(this.workspaceRoot));
        let existingTasks = config.get<any[]>('tasks', []);
        
        for (const task of tasks) {
            // 检查是否已存在同名任务
            let finalLabel = task.label;
            const currentProjectName = path.basename(task.file, path.extname(task.file));
            
            // 查找同名任务
            const duplicateTasks = existingTasks.filter(t => t.label === task.label);
            
            if (duplicateTasks.length > 0) {
                // 检查是否有来自不同项目的同名任务
                const differentProjectTask = duplicateTasks.find(t => {
                    const taskProjectName = path.basename(t.file, path.extname(t.file));
                    return taskProjectName !== currentProjectName;
                });
                
                if (differentProjectTask) {
                    // 如果有来自不同项目的同名任务，添加项目名后缀
                    finalLabel = `${task.label}-${currentProjectName}`;
                    
                    // 检查添加项目名后缀后是否仍然重复
                    if (existingTasks.some(t => t.label === finalLabel)) {
                        console.warn(`Task "${finalLabel}" already exists. Skipping task creation.`);
                        continue; // 跳过这个任务，不添加
                    }
                } else {
                    // 如果所有重名任务都来自同一项目，跳过这个任务
                    console.warn(`Task "${task.label}" already exists for this project. Skipping task creation.`);
                    continue; // 跳过这个任务，不添加
                }
            }
            
            // 更新任务的label
            task.label = finalLabel;
            existingTasks.push(task);
        }
        
        await config.update('tasks', existingTasks, vscode.ConfigurationTarget.WorkspaceFolder);
    }

    /**
     * 创建默认模板目录结构
     */
    async initializeDefaultTemplates(): Promise<void> {
        const templateDir = path.join(this.workspaceRoot, ProjectTemplateManager.DEFAULT_TEMPLATE_DIR);
        
        if (!fs.existsSync(templateDir)) {
            fs2.ensureDirSync(templateDir);
            
            // 创建示例模板
            await this.createExampleTemplate(templateDir);
        }
    }

    /**
     * 创建示例模板
     */
    private async createExampleTemplate(templateDir: string): Promise<void> {
        const exampleDir = path.join(templateDir, 'console-app');
        fs2.ensureDirSync(exampleDir);
        
        // 创建模板配置文件
        const templateConfig = {
            name: 'Console Application',
            description: 'A simple console application template',
            files: [
                {
                    source: 'main.lpr',
                    target: 'main.lpr',
                    cursorPosition: { line: 6, column: 4 }
                },
                {
                    source: 'README.md',
                    target: 'README.md'
                }
            ],
            tasks: [
                {
                    label: 'debug',
                    file: 'main.lpr',
                    type: 'fpc',
                    presentation: {
                        showReuseMessage: false,
                        clear: true,
                        revealProblems: 'onProblem'
                    },
                    buildOption: {
                        unitOutputDir: './out',
                        customOptions: ['-dDEBUG', '-gw2']
                    }
                },
                {
                    label: 'release',
                    file: 'main.lpr',
                    type: 'fpc',
                    presentation: {
                        showReuseMessage: false,
                        clear: true,
                        revealProblems: 'onProblem'
                    },
                    buildOption: {
                        unitOutputDir: './out',
                        customOptions: ['-dRELEASE', '-O2']
                    }
                }
            ]
        };
        
        fs.writeFileSync(
            path.join(exampleDir, 'template.json'),
            JSON.stringify(templateConfig, null, 2)
        );
        
        // 创建主程序文件
        const mainContent = `program main;
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

begin
  WriteLn('Hello, World!');
  WriteLn('Created on: {{DATE}} at {{TIME}}');
  WriteLn('Author: {{USER}}');
  
  // Your code here
  
  ReadLn;
end.`;
        
        fs.writeFileSync(path.join(exampleDir, 'main.lpr'), mainContent);
        
        // 创建README文件
        const readmeContent = `# {{PROJECT_NAME}}

Created on {{DATE}} by {{USER}}

## Description

This is a console application created from template.

## Build

Use the debug or release build configuration in VS Code.

## Run

After building, run the executable from the out directory.
`;
        
        fs.writeFileSync(path.join(exampleDir, 'README.md'), readmeContent);
    }
}