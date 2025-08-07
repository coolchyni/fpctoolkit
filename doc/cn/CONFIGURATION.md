# 扩展配置

FreePascal Toolkit 扩展提供了多种可在 VS Code 设置中配置的选项：

## 环境设置
设置项 | 类型 | 描述 |
--------|------|-------------|
`fpctoolkit.env.PP` | string | 编译器位置 (例如：`/usr/bin/fpc`, `/usr/bin/ppc386`)
`fpctoolkit.env.FPCDIR` | string | Free Pascal 源代码位置 (例如：`/usr/local/share/fpcsrc`)
`fpctoolkit.env.LAZARUSDIR` | string | Lazarus 源代码位置 (例如：`/usr/local/share/lazsrc`)
`fpctoolkit.env.FPCTARGET` | string | 目标操作系统 (例如：`win32`, `win64`, `linux`, `darwin`)
`fpctoolkit.env.FPCTARGETCPU` | string | 目标CPU族 (例如：`x86_64`)

## 语言服务器设置
设置项 | 类型 | 描述 |
--------|------|-------------|
`fpctoolkit.pasls.path` | string | Pascal 语言服务器 (pasls) 文件位置
`fpctoolkit.lsp.trace.server` | string | 跟踪 VS Code 和 pascal 语言服务器之间的通讯信息
`fpctoolkit.lsp.initializationOptions.program` | string | 主程序文件。如果未指定，则使用当前文件
`fpctoolkit.lsp.initializationOptions.overloadPolicy` | enum | 指定重复的函数或定义如何显示
`fpctoolkit.lsp.initializationOptions.maximumCompletions` | number | 自动代码提示显示的最大数量
`fpctoolkit.lsp.initializationOptions.insertCompletionsAsSnippets` | boolean | 函数或过程参数自动变成模版插入
`fpctoolkit.lsp.initializationOptions.insertCompletionProcedureBrackets` | boolean | 有参数的过程或函数插入时自动忽略参数
`fpctoolkit.lsp.initializationOptions.includeWorkspaceFoldersAsUnitPaths` | boolean | 将当前工作目录添加到单元文件搜索目录 (-Fu)
`fpctoolkit.lsp.initializationOptions.includeWorkspaceFoldersAsIncludePaths` | boolean | 将当前工作目录添加到 include 目录 (-Fi)
`fpctoolkit.lsp.initializationOptions.checkSyntax` | boolean | 文件打开和保存时进行语法检查
`fpctoolkit.lsp.initializationOptions.publishDiagnostics` | boolean | 语法错误作为诊断信息显示
`fpctoolkit.lsp.initializationOptions.workspaceSymbols` | boolean | 允许显示工作目录的类，函数，过程
`fpctoolkit.lsp.initializationOptions.documentSymbols` | boolean | 允许显示当前文档的类，函数，过程
`fpctoolkit.lsp.initializationOptions.minimalisticCompletions` | boolean | 自动代码提示包含最少的信息
`fpctoolkit.lsp.initializationOptions.showSyntaxErrors` | boolean | 弹窗显示语法错误提示

## 格式化设置
设置项 | 类型 | 描述 |
--------|------|-------------|
`fpctoolkit.format.enabled` | boolean | 启用源码格式化 (使用 JCF)
`fpctoolkit.format.tabsize` | number | 制表符(tab)转空格数
`fpctoolkit.format.cfgpath` | string | 格式化配置文件路径 (jcfsettings.cfg)

## 通用设置
设置项 | 类型 | 描述 |
--------|------|-------------|
`fpctoolkit.searchPath` | string[] | 单元文件搜索路径 (-Fu)
`fpctoolkit.libPath` | string[] | 库搜索路径 (-Fl)
`fpctoolkit.customOptions` | string[] | 自定义选项
`fpctoolkit.debug.autoBuild` | boolean | 当文件有更改时，在调试前自动编译默认项目
`fpctoolkit.lazarus.enabled` | boolean | 启用 Lazarus 项目支持，包括 .lpi 文件和 Lazarus 特定功能

## MCP 服务器设置
设置项 | 类型 | 描述 |
--------|------|-------------|
`fpctoolkit.mcp.enabled` | boolean | 启用模型上下文协议(MCP)服务器，为AI助手提供编译命令信息
`fpctoolkit.mcp.autoRegister` | boolean | 启动时自动向 Claude Desktop 等外部应用程序注册 MCP 服务器
`fpctoolkit.mcp.port` | number | MCP 服务器端口 (0表示自动分配)
`fpctoolkit.mcp.host` | string | MCP 服务器主机地址

## 如何配置

您可以通过多种方式配置这些设置：

### VS Code 设置界面
1. 打开 VS Code 设置 (`Ctrl+,` 或 `Cmd+,`)
2. 搜索 "fpctoolkit"
3. 修改所需的设置

### 设置 JSON
在您的 `settings.json` 文件中添加配置：

```json
{
  "fpctoolkit.env.PP": "/usr/bin/fpc",
  "fpctoolkit.format.enabled": true,
  "fpctoolkit.mcp.enabled": true,
  "fpctoolkit.lsp.initializationOptions.checkSyntax": true
}
```

### 工作区设置
对于项目特定的设置，在您的工作区中创建 `.vscode/settings.json` 文件：

```json
{
  "fpctoolkit.searchPath": ["./lib", "./units"],
  "fpctoolkit.customOptions": ["-dDEBUG"]
}
```