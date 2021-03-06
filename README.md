# FreePascal Toolkit 
`FreePascal Toolkit` is a VSCode extension for developing FreePascal applications.

![FreePascal Toolkit preview](images/fpctoolkit.gif)

## Requirements

- Install [FreePascal](https://www.freepascal.org/download.var) on your system. Then Set the system PATH environment variable, or set `fpctoolkit.fpcPath` in vscode's user setting. 

- Install [GDB Debugger - Beyond](https://marketplace.visualstudio.com/items?itemName=coolchyni.beyond-debug) for  debug freepascal program .

- Install [OmniPascal](https://marketplace.visualstudio.com/items?itemName=Wosi.omnipascal) for  use code completion. 
For a better experience, please turn off it's auto  create buildscripts feature!
    ~~~js
    "omnipascal.createBuildScripts": false
    ~~~

## Features
- build freepascal program
- explorer build tasks


## Freepascal Task Settings

You can add freepascal compile tasks by editing `task.json` in your .vscode folder.

The task's type is `fpc`. It contains the following configuration items.

### Task settings
Field   | type  |  Description  |
------  | ----- |  :-------------
`file`  | string|main program file
`type`  | string|always be `fpc`
`buildOption`|object|build options

### buildOptions
Field  | type | Description  |
-------| ---- |:---------------
`targetOS`  | string | Target OS (-T).  eg. `linux` `win64`
`targetCPU` |tring| Target cpu family (-P). eg. `x86_64` `i386`
`customOptions`|string []| Any compile options for fpc.     
`libPath`|string[]|Searchpath for libraries.(-Fl)
`outputFile`| string| Target file name.(-o)
`unitOutputDir`| string|Unit output directory.(-FU)
`optimizationLevel`| number|Optimization levels (-O)
`searchPath`| string[]|Searchpath for units and other system dependent things.(-Fu)
`syntaxMode`| string|Syntax Mode (-M)

example:
~~~json
{
	"version": "2.0.0",
	"tasks": [
		{
			"label": "fpc:build:main",
			"file": "main.lpr",
			"type": "fpc",
			"buildOption": {
				"unitOutputDir": "./out",
				"customOptions": [
					"-dDEBUG"
				]
			}
		}
	]
}
~~~

## Release Notes

[view changelog](CHANGELOG.md)


