# FPC Toolkit Project Template Guide

## Overview

FPC Toolkit now supports a template-based project creation system that allows users to quickly create new projects using predefined templates and supports custom templates.

## Features

- **Built-in Default Templates**: Includes standard FPC project templates
- **Custom Template Support**: Users can create and use their own project templates
- **Template Variable Substitution**: Supports automatic replacement of variables like date, time, username, etc.
- **Multi-file Templates**: Supports complex project templates containing multiple files
- **Task Configuration**: Templates can include pre-configured build tasks

## Usage

### Creating New Projects

1. Click the "+" button in the FPC Projects view, or use the command `FpcToolkit: Create New Project`
2. Select a template from the available template list
3. The project will be created based on the template, including files and build configuration

### Initializing Template Directory

If no templates are found, the system will prompt to initialize the default template directory:

1. Select "Initialize Templates"
2. The system will create a `templates` folder in the workspace root directory
3. Includes a sample console application template

## Custom Templates

### Template Directory Structure

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

### Template Configuration File (template.json)

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

### Configuration Field Descriptions

- **name**: Template display name
- **description**: Template description
- **files**: Template file list
  - **source**: Source file name in template directory
  - **target**: Target file name when creating project
  - **cursorPosition**: Optional, cursor position when file is opened
- **tasks**: Optional, pre-configured build tasks

### Template Variables

The following variables can be used in template files and will be automatically replaced when creating projects:

- `{{DATE}}`: Current date
- `{{TIME}}`: Current time
- `{{YEAR}}`: Current year
- `{{MONTH}}`: Current month (two digits)
- `{{DAY}}`: Current day (two digits)
- `{{USER}}`: Current username
- `{{PROJECT_NAME}}`: Project name

### Example Template Files

#### Simple Console App (templates/simple-console/main.lpr)

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

#### Program with Unit (templates/unit-with-program/main.lpr)

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

#### Custom Unit File (templates/unit-with-program/myunit.pas)

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

## Configuration Options

### Custom Template Directories

Configure `fpctoolkit.project.templateDirectories` in VS Code settings to specify additional template search directories:

```json
{
  "fpctoolkit.project.templateDirectories": [
    "/path/to/my/templates",
    "./project-templates",
    "~/my-pascal-templates"
  ]
}
```

Supports:
- Absolute paths
- Paths relative to workspace root
- User home directory paths (~)

## Commands

- `fpctoolkit.project.newproject`: Create new project (shows template selection)
- `fpctoolkit.project.newfromtemplate`: Create new project from template (alias)
- `fpctoolkit.project.inittemplatedir`: Initialize default template directory

## Best Practices

1. **Template Naming**: Use descriptive template names
2. **File Organization**: Keep related files in the same template directory
3. **Task Configuration**: Provide appropriate build configurations for templates
4. **Documentation**: Include README files in templates explaining usage
5. **Variable Usage**: Use template variables appropriately to improve reusability

## Migration Guide

Migrating from old project creation methods:

1. The old "New FPC Project" functionality is now provided as a built-in template
2. "New Custom Project" functionality is replaced by the template system
3. Existing projects are not affected, only new project creation is impacted

## Troubleshooting

### Templates Not Showing
- Check if template directory exists
- Confirm template.json file format is correct
- Check VS Code developer console for error messages

### File Creation Failed
- Check file paths and permissions
- Confirm template files exist and are readable
- Verify target directory is writable

### Task Configuration Issues
- Check tasks.json syntax
- Confirm build options are correct
- Verify file path references

## Testing New Features

### Test Steps

1. **Open VS Code and Load Project**
   - Make sure you're in the FPC Toolkit project root directory

2. **Test Template System**
   - Press F5 to start extension development host
   - Open an empty workspace in the new window
   - Click the "+" button in FPC Projects view
   - Should see available template list including:
     - "New FPC Project" (built-in default template)
     - "Simple Console App" (if templates directory exists)
     - "Program with Unit" (if templates directory exists)

3. **Create Project**
   - Select a template
   - Project files should be created
   - Template variables should be correctly replaced
   - Build tasks should be added to tasks.json

4. **Test Initialization Feature**
   - In a workspace without templates
   - Run "Create New Project" command
   - Should prompt to initialize template directory
   - Select "Initialize Templates"
   - Should create default template directory and sample templates

### Feature Verification

- ✅ Template list displays correctly
- ✅ File creation successful
- ✅ Variable substitution works normally
- ✅ Task configuration added correctly
- ✅ Cursor position set correctly
- ✅ Custom template directory support
- ✅ Initialization feature works normally

## Known Limitations

1. **Template Validation**: Currently no strict template format validation
2. **Error Handling**: Error messages when template loading fails may not be detailed enough
3. **Variable Expansion**: Currently only supports basic date/time/user variables
4. **File Overwrite**: Does not check if files already exist when creating projects

## Future Improvements

1. **More Variables**: Support project name, workspace path and other variables
2. **Template Validation**: Add template format validation and error reporting
3. **Template Management**: Provide template management interface
4. **Online Templates**: Support downloading templates from remote repositories
5. **Template Wizard**: Provide interactive template creation wizard