# Extension Configuration

The FreePascal Toolkit extension provides various configuration options that can be set in VS Code settings:

## Environment Settings
Setting | Type | Description |
--------|------|-------------|
`fpctoolkit.env.PP` | string | Compiler location (e.g., `/usr/bin/fpc`, `/usr/bin/ppc386`)
`fpctoolkit.env.FPCDIR` | string | Free Pascal source code location (e.g., `/usr/local/share/fpcsrc`)
`fpctoolkit.env.LAZARUSDIR` | string | Lazarus source code location (e.g., `/usr/local/share/lazsrc`)
`fpctoolkit.env.FPCTARGET` | string | Target operating system (e.g., `win32`, `win64`, `linux`, `darwin`)
`fpctoolkit.env.FPCTARGETCPU` | string | Target CPU family (e.g., `x86_64`)

## Language Server Settings
Setting | Type | Description |
--------|------|-------------|
`fpctoolkit.pasls.path` | string | Pascal Language Server (pasls) file location
`fpctoolkit.lsp.trace.server` | string | Trace communication between VS Code and pascal language server
`fpctoolkit.lsp.initializationOptions.program` | string | Main program file. If not specified, uses current file
`fpctoolkit.lsp.initializationOptions.overloadPolicy` | enum | Specify how duplicate functions or definitions are displayed
`fpctoolkit.lsp.initializationOptions.maximumCompletions` | number | Maximum number of auto code suggestions displayed
`fpctoolkit.lsp.initializationOptions.insertCompletionsAsSnippets` | boolean | Function or procedure parameters automatically become template insertions
`fpctoolkit.lsp.initializationOptions.insertCompletionProcedureBrackets` | boolean | Automatically ignore parameters when inserting procedures or functions with parameters
`fpctoolkit.lsp.initializationOptions.includeWorkspaceFoldersAsUnitPaths` | boolean | Add current working directory to unit file search directories (-Fu)
`fpctoolkit.lsp.initializationOptions.includeWorkspaceFoldersAsIncludePaths` | boolean | Add current working directory to include directories (-Fi)
`fpctoolkit.lsp.initializationOptions.checkSyntax` | boolean | Perform syntax checking when files are opened and saved
`fpctoolkit.lsp.initializationOptions.publishDiagnostics` | boolean | Display syntax errors as diagnostic information
`fpctoolkit.lsp.initializationOptions.workspaceSymbols` | boolean | Allow displaying classes, functions, procedures from workspace
`fpctoolkit.lsp.initializationOptions.documentSymbols` | boolean | Allow displaying classes, functions, procedures from current document
`fpctoolkit.lsp.initializationOptions.minimalisticCompletions` | boolean | Auto code suggestions contain minimal information
`fpctoolkit.lsp.initializationOptions.showSyntaxErrors` | boolean | Show syntax error prompts in popup

## Formatting Settings
Setting | Type | Description |
--------|------|-------------|
`fpctoolkit.format.enabled` | boolean | Enable source code formatting (using JCF)
`fpctoolkit.format.tabsize` | number | Number of spaces to convert tab to
`fpctoolkit.format.cfgpath` | string | Format configuration file path (jcfsettings.cfg)

## General Settings
Setting | Type | Description |
--------|------|-------------|
`fpctoolkit.searchPath` | string[] | Unit file search path (-Fu)
`fpctoolkit.libPath` | string[] | Library search path (-Fl)
`fpctoolkit.customOptions` | string[] | Custom options
`fpctoolkit.debug.autoBuild` | boolean | Automatically compile default project before debugging when files have changes
`fpctoolkit.lazarus.enabled` | boolean | Enable Lazarus project support for .lpi files and Lazarus-specific features

## MCP Server Settings
Setting | Type | Description |
--------|------|-------------|
`fpctoolkit.mcp.enabled` | boolean | Enable Model Context Protocol (MCP) server to provide compilation command information for AI assistants
`fpctoolkit.mcp.autoRegister` | boolean | Automatically register MCP server with external applications like Claude Desktop on startup
`fpctoolkit.mcp.port` | number | MCP server port (0 means auto-assign)
`fpctoolkit.mcp.host` | string | MCP server host address

## How to Configure

You can configure these settings in several ways:

### VS Code Settings UI
1. Open VS Code Settings (`Ctrl+,` or `Cmd+,`)
2. Search for "fpctoolkit"
3. Modify the desired settings

### Settings JSON
Add configurations to your `settings.json` file:

```json
{
  "fpctoolkit.env.PP": "/usr/bin/fpc",
  "fpctoolkit.format.enabled": true,
  "fpctoolkit.mcp.enabled": true,
  "fpctoolkit.lsp.initializationOptions.checkSyntax": true
}
```

### Workspace Settings
For project-specific settings, create a `.vscode/settings.json` file in your workspace:

```json
{
  "fpctoolkit.searchPath": ["./lib", "./units"],
  "fpctoolkit.customOptions": ["-dDEBUG"]
}
```