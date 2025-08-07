# Lazbuild Usage Guide

This extension supports compiling Lazarus projects using both `lazbuild` and `fpc` compilers with automatic fallback.

## Overview

When building a Lazarus project (`.lpi` or `.lpr` files), the extension follows this priority:

1. **Primary**: Use `lazbuild` if available and preferred
2. **Fallback**: Use `fpc` compiler with extracted build options

## Configuration

### Prefer lazbuild

You can control whether to prefer `lazbuild` over `fpc` using the configuration:

```json
{
    "fpctoolkit.lazarus.preferLazbuild": true
}
```

- `true` (default): Try `lazbuild` first, fallback to `fpc`
- `false`: Always use `fpc` compiler

### Lazbuild Detection

The extension automatically detects `lazbuild` in the following order:

1. Check if `lazbuild` is available in system PATH
2. Check common installation paths:
   - **Windows**: `C:\lazarus\lazbuild.exe`, `C:\Program Files\Lazarus\lazbuild.exe`
   - **macOS**: `/usr/local/bin/lazbuild`, `/Applications/Lazarus/lazbuild`
   - **Linux**: `/usr/bin/lazbuild`, `/usr/local/bin/lazbuild`
3. Check `LAZARUSDIR` environment variable or `fpctoolkit.env.LAZARUSDIR` setting

## Build Modes

When using `lazbuild`, the extension:

- Passes the correct build mode using `--build-mode=<mode>`
- Uses `--build-all` for rebuild operations
- Adds `--quiet` flag to reduce output noise

When using `fpc` fallback, the extension:

- Extracts build options from the Lazarus project file
- Converts Lazarus-specific settings to FPC command-line options
- Maintains compatibility with existing FPC workflows

## Benefits of lazbuild

Using `lazbuild` provides several advantages:

1. **Native Lazarus Support**: Handles Lazarus-specific build configurations automatically
2. **Build Mode Support**: Properly processes different build modes defined in `.lpi` files
3. **Dependency Management**: Automatically handles package dependencies
4. **Optimized Compilation**: Uses Lazarus's optimized build process

## Usage Examples

### Building with lazbuild
When `lazbuild` is available and preferred:
```
Using lazbuild compiler
lazbuild --build-mode=Release --quiet MyProject.lpi
```

### Fallback to fpc
When `lazbuild` is unavailable or not preferred:
```
Using fpc compiler
fpc MyProject.lpr -MObjFPC -Scghi -O1 -g -gl -l -vewnhibq -Fu./lib -FU./out
```

## Troubleshooting

### lazbuild not found

If `lazbuild` is not detected:

1. Ensure Lazarus is properly installed
2. Add Lazarus installation directory to your system PATH
3. Set the `LAZARUSDIR` environment variable
4. Configure `fpctoolkit.env.LAZARUSDIR` in VS Code settings

### Compilation Errors

The extension provides detailed error reporting for both compilers:

- **lazbuild errors**: Parsed from lazbuild output and displayed in Problems panel
- **fpc errors**: Standard FPC error parsing with line/column information

### Fallback Behavior

If `lazbuild` fails or is unavailable, the extension automatically falls back to `fpc` with:

- Extracted compiler options from the Lazarus project
- Converted build settings
- Standard FPC error reporting

## Environment Variables

The extension respects the following environment variables:

- `LAZARUSDIR`: Path to Lazarus installation directory
- `PP`: Path to FPC compiler (fallback)
- `FPCDIR`: Path to FPC source directory

## Command Line Options

### lazbuild Options Used

- `--build-mode=<mode>`: Specify build mode
- `--build-all`: Force rebuild all units
- `--quiet`: Reduce output verbosity

### FPC Fallback Options

The extension automatically converts Lazarus project settings to appropriate FPC command-line options:

- `-M<mode>`: Syntax mode
- `-T<target>`: Target OS
- `-P<cpu>`: Target CPU
- `-Fu<path>`: Unit search paths
- `-Fi<path>`: Include search paths
- `-Fl<path>`: Library search paths
- `-FU<path>`: Unit output directory
- `-o<file>`: Output file name