# FreePascal Toolkit 
`FreePascal Toolkit` 是一个用于开发FreePascal程序的 VSCode 应用扩展.

![FreePascal Toolkit preview](images/doc/fpctoolkit.gif)

[English Document](README.md)
## 准备条件

- 安装 [Lazarus](https://www.lazarus-ide.org/) 或 [FreePascal](https://www.freepascal.org/download.var) . 
- 默认会自动搜索FPC路径，如果找不到，请设置FPC的路径为系统搜索路径，或者在插件里设置 `fpctoolkit.env.PP = FPC程序全路径`. 

- 安装并使用 [GDB Debugger - Beyond](https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug) 进行程序调试

## 功能特性

### 核心功能
- **项目管理** - 构建、重建、清理 FreePascal 和 Lazarus 程序
- **项目模板** - 基于内置或自定义模板创建新项目，支持变量替换
- **Lazarus LPI 支持** - 完整的项目文件支持，多模式构建管理
- **高级构建系统** - 自定义任务继承，跨平台编译支持
- **代码智能** - 自动补全、跳转定义、符号引用
- **代码模板** - 丰富的代码片段，快速开发
- **语法高亮** - Pascal 语法高亮，支持条件编译
- **代码格式化** - 集成 [jcf cli](https://github.com/coolchyni/jcf-cli) 格式化工具
- **快速修复** - 智能建议和自动修正
- **重构工具** - 符号重命名和代码操作

### 详细功能说明

#### 项目与构建管理
- **项目模板**: 基于模板的项目创建系统
  - 内置控制台应用和模块化程序模板
  - 自定义模板支持，变量替换（日期、时间、用户名、项目名）
  - 多文件模板，预配置构建任务
  - 模板初始化和管理命令
- **Lazarus LPI 项目支持**: 自动检测并解析 `.lpi` 项目文件，提取所有构建模式，支持多模式构建管理
- **高级任务系统**: 
  - 自定义构建任务，支持从其他任务继承
  - 支持自定义构建选项（目标平台/CPU、输出目录、编译参数等）
  - 跨平台和多架构编译（Linux/Windows/macOS，x86_64/i386 等）
  - 构建前/后事件支持，便于自动化和集成
  - FreePascal 和 Lazarus 项目混合管理，自动识别最佳体验

#### 代码智能与导航
- **智能代码补全**: 支持函数、过程、类、变量、关键字的智能补全
 
	![](images/doc/code-snippets.gif)
 
- **代码导航**: 
  - 使用 `Ctrl+Up`、`Ctrl+Down` 在函数声明和函数体之间快速跳转
  - 类和函数引用，支持文档大纲
	
	![](images/doc/documentsymbol.gif) 

#### 代码质量与格式化
- **语法高亮**: 
  - 高级 Pascal 语法高亮显示
  - 根据编译条件识别 `{$IFDEF} {$ELSE} {$ENDIF}` 等条件编译
- **代码格式化**: 
	
	![](images/doc/format.gif) 
	
	- 使用集成的 jcf cli 格式化源代码
	- 使用 `jcfsettings.cfg` 作为配置文件，如果安装了 Lazarus 将自动使用其配置

#### 开发效率工具
- **快速修复**: 针对常见问题的智能建议
	- 快速修复警告: `(5025) Local variable "xxx" not used`
	
	![](images/doc/quickfix.gif) 

- **重构工具**:
	- 自动重命名符号（函数、类、变量及其相关引用）
	- 使用 `Ctrl+Shift+C` 自动实现函数体
	- 删除无用的单元声明，代码清理
- **代码模板**: 丰富的代码片段，包括 class、if-else、begin-end 等模板

### 项目模板

使用内置或自定义模板快速创建新的 Pascal 项目：

#### 内置模板
- **简单控制台应用**: 基础控制台应用程序，包含问候消息
- **带单元的程序**: 演示模块化编程，包含独立的单元文件

#### 模板功能
- **变量替换**: 自动替换 `{{DATE}}`、`{{TIME}}`、`{{USER}}`、`{{PROJECT_NAME}}`
- **多文件支持**: 模板可以包含多个源文件、文档和配置
- **预配置任务**: 模板包含调试和发布构建配置
- **自定义模板**: 在 `templates/` 目录中创建自己的模板

#### 使用方法
1. 在 FPC Projects 视图中点击 "+" 按钮或使用 `Ctrl+Shift+P` → "FpcToolkit: Create New Project"
2. 从可用模板中选择
3. 项目文件创建时变量自动替换
4. 构建任务已配置并可直接使用

详细的模板创建和自定义说明，请参阅[模板指南](doc/cn/TEMPLATE_GUIDE.md)。

## Pascal Language Server 

[pascal-language-server](https://github.com/coolchyni/pascal-language-server)是一个[Free Pascal](https://www.freepascal.org/)实现的[LSP](https://microsoft.github.io/language-server-protocol/) 服务端程序。使用[Lazarus](https://www.lazarus-ide.org/)的[CodeTools](https://wiki.lazarus.freepascal.org/Codetools)作为后台。

## 任务设置

你可以在.vscode目录下编辑 `task.json` 文件来修改任务选项目。

任务类型为 `fpc`. 包含以下可用的配置项:

### Task 选项
字段   | 类型  |  描述  |
------  | ----- |  :-------------
file  | string|Free Pascal 项目文件. (.lpr,.dpr)
type  | string|必须是 `fpc`
cwd   | string|当前工作目录. 如果不设置默认使用vscode工作目录.
cleanExt|string|指定执行清理项目时需要清除的文件名后缀. 输入* 表示清除所有文件. 默认(.o,.ppu,.lfm,.a,.or,.res)
buildOption|object|build 选项
inherited|string| 被继承的任务名

### buildEvent
字段   | 类型  |  描述  |
-------| ---- |:---------------
before_build  | string[] | 编译前执行的命令
after_build_success | string[]| 编译成功后执行的命令
after_build_failure | string []|编译失败后执行的命令

### buildOptions
字段   | 类型  |  描述  |
-------| ---- |:---------------
targetOS  | string | 目标操作系统 (-T).  eg. `linux` `win64`
targetCPU |string| 目标CPU族 (-P). eg. `x86_64` `i386`
customOptions|string []| 自定义编译选项.     
libPath|string[]|库搜索路径.(-Fl)
outputFile| string| 目标文件名称.(-o)
unitOutputDir| string|单元文件输出路径.(-FU)
optimizationLevel| number|代码优化级别 (-O)
searchPath| string[]|单元文件搜索路径.(-Fu)
syntaxMode| string|语法模式 (-M {$mode})
forceRebuild| boolean|强制更新所有单元文件并编译.(-B)
msgIgnore|number[]|指定编译时哪些消息不需要显示.(-vmxxx)

配置示例:
~~~ json
{
	"version": "2.0.0",
	"tasks": [
		{
			"label": "debug",
			"file": "main.lpr",
			"type": "fpc",
			"buildOption": {
				"unitOutputDir": "./out",
				"customOptions": [
					"-dDEBUG",
					"-gw2"
				]
			}
		}
	]
}
~~~

## 文档

详细文档请参阅：
- [配置指南](doc/cn/CONFIGURATION.md) (中文) / [Configuration Guide](doc/en/CONFIGURATION.md) (English)
- [模板指南](doc/cn/TEMPLATE_GUIDE.md) (中文) / [Template Guide](doc/en/TEMPLATE_GUIDE.md) (English)
- [Lazbuild 使用指南](doc/cn/LAZBUILD_USAGE.md) (中文) / [Lazbuild Usage Guide](doc/en/LAZBUILD_USAGE.md) (English)

# 致谢
## 语法高亮

语法文件来自于 https://github.com/maresmar/ST-Pascal

## 格式化

修改自  https://github.com/git-bee/jcf-cli

## Pascal-language-server

修改自
https://github.com/genericptr/pascal-language-server 
https://github.com/arjanadriaanse/pascal-language-server

# 发布说明

[查看更新记录](CHANGELOG.md)


