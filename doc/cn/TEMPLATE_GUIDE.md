# FPC Toolkit 项目模板指南

## 概述

FPC Toolkit 现在支持基于模板的项目创建系统，允许用户使用预定义的模板快速创建新项目，并支持自定义模板。

## 功能特性

- **内置默认模板**: 包含标准的 FPC 项目模板
- **自定义模板支持**: 用户可以创建和使用自己的项目模板
- **模板变量替换**: 支持日期、时间、用户名等变量自动替换
- **多文件模板**: 支持包含多个文件的复杂项目模板
- **任务配置**: 模板可以包含预配置的构建任务

## 使用方法

### 创建新项目

1. 在 FPC Projects 视图中点击 "+" 按钮，或使用命令 `FpcToolkit: Create New Project`
2. 从可用模板列表中选择一个模板
3. 项目将根据模板创建，包括文件和构建配置

### 初始化模板目录

如果没有找到模板，系统会提示初始化默认模板目录：

1. 选择 "Initialize Templates" 
2. 系统会在工作区根目录创建 `templates` 文件夹
3. 包含一个示例控制台应用程序模板

## 自定义模板

### 模板目录结构

```
templates/
├── console-app/
│   ├── template.json
│   ├── main.lpr
│   └── README.md
└── my-custom-template/
    ├── template.json
    ├── program.lpr
    ├── unit1.pas
    └── config.ini
```

### 模板配置文件 (template.json)

```json
{
  "name": "Console Application",
  "description": "A simple console application template",
  "files": [
    {
      "source": "main.lpr",
      "target": "main.lpr",
      "cursorPosition": { "line": 6, "column": 4 }
    },
    {
      "source": "README.md",
      "target": "README.md"
    }
  ],
  "tasks": [
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
        "customOptions": ["-dDEBUG", "-gw2"]
      }
    }
  ]
}
```

### 配置字段说明

- **name**: 模板显示名称
- **description**: 模板描述
- **files**: 模板文件列表
  - **source**: 模板目录中的源文件名
  - **target**: 创建项目时的目标文件名
  - **cursorPosition**: 可选，文件打开时光标位置
- **tasks**: 可选，预配置的构建任务

### 模板变量

模板文件中可以使用以下变量，创建项目时会自动替换：

- `{{DATE}}`: 当前日期
- `{{TIME}}`: 当前时间  
- `{{YEAR}}`: 当前年份
- `{{MONTH}}`: 当前月份 (两位数)
- `{{DAY}}`: 当前日期 (两位数)
- `{{USER}}`: 当前用户名
- `{{PROJECT_NAME}}`: 项目名称

### 示例模板文件

#### 简单控制台应用 (templates/simple-console/main.lpr)

```pascal
program {{PROJECT_NAME}};
{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

begin
  WriteLn('Hello from {{PROJECT_NAME}}!');
  WriteLn('Created on: {{DATE}} at {{TIME}}');
  WriteLn('Author: {{USER}}');
  
  // Add your code here
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

#### 带单元的程序 (templates/unit-with-program/main.lpr)

```pascal
program {{PROJECT_NAME}};
{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  {{PROJECT_NAME}}unit;

begin
  WriteLn('Program with Unit Example');
  WriteLn('Created by {{USER}} on {{DATE}}');
  
  // Call function from our unit
  SayHello('{{USER}}');
  
  WriteLn('Current year: ', GetCurrentYear);
  
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

#### 自定义单元文件 (templates/unit-with-program/myunit.pas)

```pascal
unit {{PROJECT_NAME}}unit;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// Public procedures and functions
procedure SayHello(const Name: string);
function GetCurrentYear: Integer;

implementation

procedure SayHello(const Name: string);
begin
  WriteLn('Hello, ', Name, '! Welcome to Pascal programming.');
end;

function GetCurrentYear: Integer;
begin
  Result := CurrentYear;
end;

end.
```

## 配置选项

### 自定义模板目录

在 VS Code 设置中配置 `fpctoolkit.project.templateDirectories` 来指定额外的模板搜索目录：

```json
{
  "fpctoolkit.project.templateDirectories": [
    "/path/to/my/templates",
    "./project-templates",
    "~/my-pascal-templates"
  ]
}
```

支持：
- 绝对路径
- 相对于工作区根目录的路径
- 用户主目录路径 (~)

## 命令

- `fpctoolkit.project.newproject`: 创建新项目 (显示模板选择)
- `fpctoolkit.project.newfromtemplate`: 从模板创建新项目 (别名)
- `fpctoolkit.project.inittemplatedir`: 初始化默认模板目录

## 最佳实践

1. **模板命名**: 使用描述性的模板名称
2. **文件组织**: 将相关文件放在同一个模板目录中
3. **任务配置**: 为模板提供合适的构建配置
4. **文档**: 在模板中包含 README 文件说明用法
5. **变量使用**: 合理使用模板变量提高复用性

## 迁移指南

从旧的项目创建方式迁移：

1. 旧的 "New FPC Project" 功能现在作为内置模板提供
2. "New Custom Project" 功能被模板系统替代
3. 现有项目不受影响，只影响新项目的创建方式

## 故障排除

### 模板不显示
- 检查模板目录是否存在
- 确认 template.json 文件格式正确
- 查看 VS Code 开发者控制台的错误信息

### 文件创建失败
- 检查文件路径和权限
- 确认模板文件存在且可读
- 验证目标目录可写

### 任务配置问题
- 检查 tasks.json 语法
- 确认构建选项正确
- 验证文件路径引用

## 测试新功能

### 测试步骤

1. **打开 VS Code 并加载项目**
   - 确保你在 FPC Toolkit 项目的根目录

2. **测试模板系统**
   - 按 F5 启动扩展开发主机
   - 在新窗口中打开一个空的工作区
   - 在 FPC Projects 视图中点击 "+" 按钮
   - 应该看到可用的模板列表，包括：
     - "New FPC Project" (内置默认模板)
     - "Simple Console App" (如果存在 templates 目录)
     - "Program with Unit" (如果存在 templates 目录)

3. **创建项目**
   - 选择一个模板
   - 项目文件应该被创建
   - 模板变量应该被正确替换
   - 构建任务应该被添加到 tasks.json

4. **测试初始化功能**
   - 在没有模板的工作区中
   - 运行 "Create New Project" 命令
   - 应该提示初始化模板目录
   - 选择 "Initialize Templates"
   - 应该创建默认的模板目录和示例模板

### 验证功能

- ✅ 模板列表正确显示
- ✅ 文件创建成功
- ✅ 变量替换工作正常
- ✅ 任务配置正确添加
- ✅ 光标位置正确设置
- ✅ 自定义模板目录支持
- ✅ 初始化功能工作正常

## 已知限制

1. **模板验证**: 目前没有严格的模板格式验证
2. **错误处理**: 模板加载失败时的错误信息可能不够详细
3. **变量扩展**: 目前只支持基本的日期/时间/用户变量
4. **文件覆盖**: 创建项目时不会检查文件是否已存在

## 未来改进

1. **更多变量**: 支持项目名称、工作区路径等变量
2. **模板验证**: 添加模板格式验证和错误报告
3. **模板管理**: 提供模板管理界面
4. **在线模板**: 支持从远程仓库下载模板
5. **模板向导**: 提供交互式模板创建向导