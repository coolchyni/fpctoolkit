# FreePascal Toolkit
`FreePascal Toolkit` is a VS Code extension for developing FreePascal programs.

![FreePascal Toolkit preview](images/doc/fpctoolkit.gif)

For Chinese documentation, see [中文文档](README_CN.md).

## Requirements

- Install [Lazarus](https://www.lazarus-ide.org/) or [FreePascal](https://www.freepascal.org/download.var).
- The extension will automatically search for the FPC path. If not found, please add FPC to your system PATH, or set `fpctoolkit.env.PP` in the extension settings.
- Install [GDB Debugger - Beyond](https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug) for debugging.

## Features

### Core Features
- **Project Management** - Build, rebuild, and clean FreePascal and Lazarus programs
- **Project Templates** - Create new projects from built-in or custom templates with variable substitution
- **Lazarus LPI Support** - Full project file support with multi-mode build management and lazbuild integration
- **Advanced Build System** - Customizable tasks with inheritance and cross-platform compilation
- **Code Intelligence** - Auto-completion, go-to-definition, and symbol references
- **Code Snippets** - Rich templates for rapid development
- **Syntax Highlighting** - Pascal syntax with conditional compilation support
- **Code Formatting** - Integrated [jcf cli](https://github.com/coolchyni/jcf-cli) formatter
- **Quick Fixes** - Smart suggestions and auto-corrections
- **Refactoring** - Symbol renaming and code actions
- **MCP Server** - Model Context Protocol server for AI assistant integration

### Detailed Feature Overview

#### Project & Build Management
- **Project Templates**: Template-based project creation system
  - Built-in templates for console applications and modular programs
  - Custom template support with variable substitution (date, time, username, project name)
  - Multi-file templates with pre-configured build tasks
  - Template initialization and management commands
- **Lazarus LPI Project Support**: Automatically detects and parses `.lpi` project files, extracts all build modes, and supports multi-mode build management with intelligent compiler selection (lazbuild preferred, fpc fallback)
- **Advanced Task System**:
  - Customize build tasks with inheritance
  - Support for custom build options (target platform/CPU, output directory, compiler flags, etc.)
  - Cross-platform and multi-architecture compilation (Linux/Windows/macOS, x86_64/i386, etc.)
  - Pre/post build event support for automation and integration
  - Seamless management of mixed FreePascal and Lazarus projects

#### Code Intelligence & Navigation
- **Smart Auto-completion**: Intelligent completion for functions, procedures, classes, variables, and keywords

  ![](images/doc/code-snippets.gif)

- **Navigation**:
  - Use `Ctrl+Up` and `Ctrl+Down` to jump between declaration and implementation
  - Class and function references with document outline

  ![](images/doc/documentsymbol.gif)

#### Code Quality & Formatting
- **Syntax Highlighting**:
  - Advanced Pascal syntax highlighting
  - Identify compilation conditions such as `{$IFDEF} {$ELSE} {$ENDIF}`
- **Code Formatting**:

  ![](images/doc/format.gif)

  - Format source code using integrated jcf cli
  - Uses `jcfsettings.cfg` as config, or Lazarus config if installed

#### Development Productivity
- **Quick Fixes**: Smart suggestions for common issues
  - Quick fix for `(5025) Local variable "xxx" not used`

  ![](images/doc/quickfix.gif)

- **Refactoring Tools**:
  - Auto rename symbols (functions, classes, variables, and their references)
  - Use `Ctrl+Shift+C` to auto-implement procedures
  - Remove unused units and clean up code
- **Code Snippets**: Rich templates including class, if-else, begin-end, and more

### Project Templates

Create new Pascal projects quickly using built-in or custom templates:

#### Built-in Templates
- **Simple Console App**: Basic console application with greeting message
- **Program with Unit**: Demonstrates modular programming with separate unit files

#### Template Features
- **Variable Substitution**: Automatic replacement of `{{DATE}}`, `{{TIME}}`, `{{USER}}`, `{{PROJECT_NAME}}`
- **Multi-file Support**: Templates can include multiple source files, documentation, and configuration
- **Pre-configured Tasks**: Templates include debug and release build configurations
- **Custom Templates**: Create your own templates in the `templates/` directory

#### Usage
1. Click the "+" button in FPC Projects view or use `Ctrl+Shift+P` → "FpcToolkit: Create New Project"
2. Select from available templates
3. Project files are created with variables automatically replaced
4. Build tasks are configured and ready to use

For detailed template creation and customization, see the [Template Guide](doc/en/TEMPLATE_GUIDE.md).

#### AI Integration
- **MCP Server**: Model Context Protocol server for AI assistants
  - Get compile commands for Pascal files
  - Project information extraction
  - Source file analysis
  - Enable with `fpctoolkit.mcp.enabled` setting

## Pascal Language Server

Based on [pascal-language-server](https://github.com/coolchyni/pascal-language-server)

An [LSP](https://microsoft.github.io/language-server-protocol/) server implementation for Pascal variants supported by [FreePascal](https://www.freepascal.org/), including Object Pascal. It uses [CodeTools](https://wiki.lazarus.freepascal.org/Codetools) from Lazarus as the backend.

## FreePascal Task Settings

You can add FreePascal or Lazarus compile tasks by editing `tasks.json` in your .vscode folder.

The task's type is `fpc`. The following configuration options are available:

### Task Settings
Field   | Type  |  Description  |
------  | ----- |  :-------------
file  | string | Free Pascal project file (.lpr, .dpr)
type  | string | Always `fpc`
cwd   | string | Working directory. Uses workspace root if not set.
cleanExt | string | File extensions to clean when cleaning the project. Use `*` to clear all files. Default: (.o, .ppu, .lfm, .a, .or, .res)
buildOption | object | Build options
inherited | string | Inherit from another task

### Build Events
Field               | Type      | Description                                 |
--------------------|-----------|---------------------------------------------|
before_build        | string[]  | Commands to run before the build starts      |
after_build_success | string[]  | Commands to run after a successful build     |
after_build_failure | string[]  | Commands to run after a failed build         |

### Build Options
Field  | Type | Description  |
-------| ---- |:---------------
targetOS  | string | Target OS (-T), e.g. `linux`, `win64`
targetCPU | string | Target CPU family (-P), e.g. `x86_64`, `i386`
customOptions | string[] | Any compile options for FPC
libPath | string[] | Library search path (-Fl)
outputFile | string | Target file name (-o)
unitOutputDir | string | Unit output directory (-FU)
optimizationLevel | number | Optimization level (-O)
searchPath | string[] | Unit search path (-Fu)
syntaxMode | string | Syntax mode (-M)
forceRebuild | boolean | Re-compile all used units, even if the unit sources didn’t change since the last compilation (-B)
msgIgnore | number[] | List of message numbers to ignore (-vmxxx)

#### Additional Options
Field  | Type | Description  |
-------| ---- |:---------------
cleanExt | string | File extensions to clean when cleaning the project. Use `*` to clear all files. Default: (.o, .ppu, .lfm, .a, .or, .res)
inherited | string | Inherit from another task
buildEvent | object | Pre/post build event commands (e.g., `before_build`, `after_build_success`, `after_build_failure`)

#### Example
~~~json
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

## Documentation

For detailed documentation, see:
- [Configuration Guide](doc/en/CONFIGURATION.md) (English) / [配置指南](doc/cn/CONFIGURATION.md) (中文)
- [Template Guide](doc/en/TEMPLATE_GUIDE.md) (English) / [模板指南](doc/cn/TEMPLATE_GUIDE.md) (中文)
- [Lazbuild Usage Guide](doc/en/LAZBUILD_USAGE.md) (English) / [Lazbuild 使用指南](doc/cn/LAZBUILD_USAGE.md) (中文)

# Thanks
## Syntax Highlighting

Syntax files from https://github.com/maresmar/ST-Pascal

## Formatter

Modified from https://github.com/git-bee/jcf-cli

## Pascal-language-server

Modified from:
https://github.com/genericptr/pascal-language-server
https://github.com/arjanadriaanse/pascal-language-server

# Release Notes

[View changelog](CHANGELOG.md)
