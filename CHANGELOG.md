## [Unreleased]

## [1.2.3] - 2026-01-08
### add:
- add fpc global unit path to the search path

## [1.2.2] - 2025-01-06
### add:
- Optimize the handling of the Lazarus project.

## [1.2.1] - 2025-08-15
### add:
- Support for use lazbuild in Lazarus projects

## [1.2.0] - 2025-08-07
### add:
- Using lazarus project files (.lpr, .lpi) for build tasks
- Support custom templates for creating new projects
- MCP server for AI assistants
- Intelligent Lazarus compilation with lazbuild preferred and fpc fallback
- Configuration option to control lazbuild vs fpc preference
- Automatic lazbuild detection across different platforms

## [1.1.9] - 2022-12-17
### add:
-  optimize clean up and build
### fixed:
-  can't rename symbol on version 1.1.8

## [1.1.8] - 2022-12-16
### add:
-  add error handle for client to avoid crashing
-  change maximumCompletions default to 50

## [1.1.6] - 2022-12-06
### add:
-  add build events 

## [1.1.5] - 2022-11-23
### fixed:
- can't start language server on linux 

## [1.1.4] - 2022-10-15
### fixed:
- quick fix not worked 

## [1.1.3] - 2022-10-14
### add:
- code format
- quick fix for [5025] Local variable "xxx" not used
- document symbols navigation
- remove ununsed unit
### fixed:
- Enhance the stability of the program pasls 

## [1.1.0]
- pascal language server 
- code snippets
- auto completion
- gotoDeclaration, gotoDefinition
- references 
- documentHighlight
- i18n support

## [1.0.4] - 2020-10-14
### fixed:
- Throw exception when parsing non-fpc type tasks

## [1.0.3] - 2020-10-13
### fixed:
- error with "fs-extra module not found"


## [1.0.2] - 2020-10-12
### add:
- Clean menu


## [1.0.1] - 2020-09-17
### fixed:
- Fixes for issues that don't work under Linux

## [1.0.0] - 2020-09-17
- Initial release