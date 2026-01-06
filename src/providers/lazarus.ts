import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as vscode from 'vscode';
import { XMLParser } from 'fast-xml-parser';
import { CompileOption } from '../languageServer/options';
import { IProjectIntf, IProjectTask } from './projectIntf';
import { LazarusBuildModeTask } from './lazarusBuildModeTask';

export class LazarusProject implements IProjectIntf {
    // Project properties
    title: string;           // Project title
    mainFile: string;        // Main program file
    requiredPackages: string[] = []; // Required package list
    packageSearchPaths: string[] = []; // Inherited search paths from packages
    packageIncludePaths: string[] = []; // Inherited include paths from packages

    // IProjectIntf implementation
    label: string;           // Display name
    file: string;            // Project file path
    tasks: IProjectTask[] = []; // Associated tasks (build modes)

    constructor(
        title: string,
        mainFile: string,
        file: string,
        isDefault: boolean = false
    ) {
        this.title = title;
        this.mainFile = mainFile;
        this.file = file;

        // Set label based on title
        this.label = title;

        // Initialize tasks array
        this.tasks = [];
        
        // Initialize required packages array
        this.requiredPackages = [];
    }
}

/**
 * Lazarus project parsing utility class
 */
export class LazarusUtils {
    // Platform mapping
    private static platformMapping: Record<string, string> = {
        'win32': 'win32',
        'darwin': 'darwin',
        'linux': 'linux',
        'freebsd': 'freebsd',
        'openbsd': 'openbsd',
        'netbsd': 'netbsd',
        'sunos': 'solaris',
        'aix': 'aix',
        'android': 'android'
    };

    // Architecture mapping
    private static archMapping: Record<string, string> = {
        'x64': 'x86_64',
        'ia32': 'i386',
        'arm': 'arm',
        'arm64': 'aarch64',
        'ppc': 'powerpc',
        'ppc64': 'powerpc64',
        's390': 's390',
        's390x': 's390x'
    };

    // Cached system info
    private static cachedSystemInfo: {
        fpcVersion?: string;
        lazarusDir?: string;
    } = {};

    // Output channel for logging errors and warnings
    private static outputChannel: vscode.OutputChannel | null = null;

    /**
     * Initialize output channel
     */
    public static initializeOutputChannel(): void {
        if (!this.outputChannel) {
            this.outputChannel = vscode.window.createOutputChannel('FPC Toolkit: Lazarus');
        }
    }

    /**
     * Log error message to output channel
     * @param message Error message
     * @param error Error object (optional)
     */
    public static logError(message: string, error?: any): void {
        this.initializeOutputChannel();
        const timestamp = new Date().toISOString();
        let errorMessage = `[${timestamp}] ERROR: ${message}`;

        if (error) {
            if (error instanceof Error) {
                errorMessage += `\n${error.name}: ${error.message}`;
                if (error.stack) {
                    errorMessage += `\n${error.stack}`;
                }
            } else {
                errorMessage += `\n${String(error)}`;
            }
        }

        this.outputChannel?.appendLine(errorMessage);
        console.error(message, error);
    }

    /**
     * Log warning message to output channel
     * @param message Warning message
     */
    public static logWarning(message: string): void {
        this.initializeOutputChannel();
        const timestamp = new Date().toISOString();
        this.outputChannel?.appendLine(`[${timestamp}] WARNING: ${message}`);
        console.warn(message);
    }

    /**
     * Log info message to output channel
     * @param message Info message
     */
    public static logInfo(message: string): void {
        this.initializeOutputChannel();
        const timestamp = new Date().toISOString();
        this.outputChannel?.appendLine(`[${timestamp}] INFO: ${message}`);
    }

    /**
     * Get default target operating system
     * @returns Default target operating system
     */
    public static getDefaultTargetOS(): string {
        const platform = os.platform();
        return this.platformMapping[platform] || platform;
    }

    /**
     * Get default target CPU
     * @returns Default target CPU
     */
    public static getDefaultTargetCPU(): string {
        const arch = os.arch();
        return this.archMapping[arch] || arch;
    }

    /**
     * Normalize target operating system name
     * @param os Target operating system name
     * @returns Normalized operating system name
     */
    public static normalizeTargetOS(os?: string): string | undefined {
        if (!os) {
            return undefined;
        }

        const osLower = os.toLowerCase();
        const osMapping: Record<string, string> = {
            'windows': 'win32',
            'win': 'win32',
            'win64': 'win64',
            'mac': 'darwin',
            'macos': 'darwin',
            'osx': 'darwin',
            'lin': 'linux',
            'unix': 'linux',
            'freebsd': 'freebsd',
            'openbsd': 'openbsd',
            'netbsd': 'netbsd',
            'solaris': 'solaris',
            'sunos': 'solaris',
            'aix': 'aix',
            'android': 'android',
            'ios': 'ios',
            'wince': 'wince',
            'haiku': 'haiku'
        };

        return osMapping[osLower] || os;
    }

    /**
     * Normalize target CPU name
     * @param cpu Target CPU name
     * @returns Normalized CPU name
     */
    public static normalizeTargetCPU(cpu?: string): string | undefined {
        if (!cpu) {
            return undefined;
        }

        const cpuLower = cpu.toLowerCase();
        const cpuMapping: Record<string, string> = {
            'x64': 'x86_64',
            'amd64': 'x86_64',
            'x86': 'i386',
            'i386': 'i386',
            'i486': 'i386',
            'i586': 'i386',
            'i686': 'i386',
            'arm': 'arm',
            'armv7': 'arm',
            'aarch64': 'aarch64',
            'arm64': 'aarch64',
            'powerpc': 'powerpc',
            'ppc': 'powerpc',
            'powerpc64': 'powerpc64',
            'ppc64': 'powerpc64',
            'mips': 'mips',
            'mipsel': 'mipsel',
            'sparc': 'sparc',
            'sparc64': 'sparc64',
            'alpha': 'alpha',
            'ia64': 'ia64',
            's390': 's390',
            's390x': 's390x'
        };

        return cpuMapping[cpuLower] || cpu;
    }

    /**
     * Get compiler section for specific build mode from LPI file content
     * @param content LPI file content
     * @param buildModeName Build mode name
     * @returns Compiler section content, returns null if not found
     */
    public static getCompilerSectionForBuildMode(content: string, buildModeName: string): string | null {
        try {
            // Escape special regex characters in build mode name
            const escapedBuildModeName = buildModeName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

            // Pattern 1: Try to find BuildMode structure (newer Lazarus versions)
            const buildModePattern = new RegExp(
                `<BuildMode[^>]*>\\s*<Name[^>]*Value=["']${escapedBuildModeName}["'][^>]*>([\\s\\S]*?)</BuildMode>`,
                'i'
            );

            let buildModeMatch = content.match(buildModePattern);
            if (buildModeMatch) {
                const buildModeSection = buildModeMatch[1];

                // Find CompilerOptions section in this build mode
                const compilerOptionsMatch = buildModeSection.match(/<CompilerOptions[^>]*>([\s\S]*?)<\/CompilerOptions>/i);
                if (compilerOptionsMatch) {
                    return compilerOptionsMatch[1];
                }
            }

            // Pattern 2: Try to find Item structure (common in LPI files)
            const itemPattern = new RegExp(
                `<Item[^>]*Name=["']${escapedBuildModeName}["'][^>]*>([\\s\\S]*?)</Item>`,
                'i'
            );

            let itemMatch = content.match(itemPattern);
            if (itemMatch) {
                const itemSection = itemMatch[1];

                // Find CompilerOptions section in this item
                const compilerOptionsMatch = itemSection.match(/<CompilerOptions[^>]*>([\s\S]*?)<\/CompilerOptions>/i);
                if (compilerOptionsMatch) {
                    return compilerOptionsMatch[1];
                }
            }

            // Pattern 3: Try to find numbered Item structure (like <Item1>, <Item2>, etc.)
            const numberedItemPattern = new RegExp(
                `<Item\\d*[^>]*Name=["']${escapedBuildModeName}["'][^>]*>([\\s\\S]*?)</Item\\d*>`,
                'i'
            );

            itemMatch = content.match(numberedItemPattern);
            if (itemMatch) {
                const itemSection = itemMatch[1];

                // Find CompilerOptions section in this item
                const compilerOptionsMatch = itemSection.match(/<CompilerOptions[^>]*>([\s\S]*?)<\/CompilerOptions>/i);
                if (compilerOptionsMatch) {
                    return compilerOptionsMatch[1];
                }
            }

            // If not found in build mode specific section, look for global compiler options
            const globalCompilerMatch = content.match(/<CompilerOptions[^>]*>([\s\S]*?)<\/CompilerOptions>/i);
            if (globalCompilerMatch) {
                return globalCompilerMatch[1];
            }

            return null;
        } catch (error) {
            console.error(`Error getting compiler section for build mode ${buildModeName}:`, error);
            return null;
        }
    }

    /**
     * Extract target platform information and target file for specific build mode
     * @param content LPI file content
     * @param buildModeName Build mode name
     * @returns Object containing target OS, CPU and target file
     */
    public static extractTargetPlatformForBuildMode(content: string, buildModeName: string): {
        targetOS?: string;
        targetCPU?: string;
        targetFile?: string;
    } {
        const result: { targetOS?: string; targetCPU?: string; targetFile?: string } = {};

        try {
            // Get compiler section for this build mode
            const compilerSection = this.getCompilerSectionForBuildMode(content, buildModeName);

            if (compilerSection) {
                // 提取目标操作系统
                const targetOSMatch = compilerSection.match(/<TargetOS[^>]*Value=["']([^"']+)["']/i);
                if (targetOSMatch) {
                    result.targetOS = this.normalizeTargetOS(targetOSMatch[1]);
                }

                // 提取目标 CPU
                const targetCPUMatch = compilerSection.match(/<TargetCPU[^>]*Value=["']([^"']+)["']/i);
                if (targetCPUMatch) {
                    result.targetCPU = this.normalizeTargetCPU(targetCPUMatch[1]);
                }
                
                // 提取目标文件名 (从 <Target><Filename Value="..."/> 路径)
                const targetFileMatch = compilerSection.match(/<Target[^>]*>[\s\S]*?<Filename[^>]*Value=["']([^"']+)["']/i);
                if (targetFileMatch) {
                    result.targetFile = targetFileMatch[1];
                }
            }

            // 如果未指定，则使用默认值
            if (!result.targetOS) {
                result.targetOS = this.getDefaultTargetOS();
            }
            if (!result.targetCPU) {
                result.targetCPU = this.getDefaultTargetCPU();
            }
        } catch (error) {
            console.error(`Error extracting target platform for build mode ${buildModeName}:`, error);
            // 出错时使用默认值
            result.targetOS = this.getDefaultTargetOS();
            result.targetCPU = this.getDefaultTargetCPU();
        }

        return result;
    }
    /**
     * Get system defaults for target OS and CPU
     * @returns Object containing default target OS, CPU, Lazarus directory and FPC version
     */
    public static getSystemDefaults(): {
        targetOS: string,
        targetCPU: string,
        lazarusDir?: string,
        fpcVersion?: string
    } {
        this.detectSystemInfo();
        return {
            targetOS: this.getDefaultTargetOS(),
            targetCPU: this.getDefaultTargetCPU(),
            lazarusDir: this.cachedSystemInfo.lazarusDir,
            fpcVersion: this.cachedSystemInfo.fpcVersion
        };
    }

    /**
     * Detect system information (Lazarus directory, FPC version)
     */
    private static detectSystemInfo(): void {
        if (this.cachedSystemInfo.lazarusDir && this.cachedSystemInfo.fpcVersion) {
            return;
        }

        const plat = os.platform();
        let lazDir: string | undefined;
        let fpcVer: string | undefined;

        // Try to read from environmentoptions.xml
        let configPath: string | undefined;
        if (plat === 'win32') {
            const appData = process.env.APPDATA;
            if (appData) configPath = path.join(appData, 'lazarus', 'environmentoptions.xml');
        } else {
            const home = os.homedir();
            configPath = path.join(home, '.lazarus', 'environmentoptions.xml');
        }

        if (configPath && fs.existsSync(configPath)) {
            try {
                const content = fs.readFileSync(configPath, 'utf8');
                const lazDirMatch = content.match(/<LazarusDirectory[^>]*Value=["']([^"']+)["']/i);
                if (lazDirMatch) lazDir = lazDirMatch[1];

                const fpcSrcMatch = content.match(/<FPCSourceDirectory[^>]*Value=["']([^"']+)["']/i);
                if (fpcSrcMatch) fpcVer = fpcSrcMatch[1];
            } catch (err) {}
        }

        // Fallback for Mac
        if (plat === 'darwin') {
            if (!lazDir && fs.existsSync('/Applications/Lazarus')) {
                lazDir = '/Applications/Lazarus';
            }
            if (!fpcVer && fs.existsSync('/usr/local/share/fpcsrc')) {
                fpcVer = '/usr/local/share/fpcsrc';
            }
        }

        this.cachedSystemInfo.lazarusDir = lazDir;
        this.cachedSystemInfo.fpcVersion = fpcVer;
    }

    /**
     * Resolve package name to .lpk file path
     * @param packageName Package name
     * @param projectDir Project directory
     * @returns .lpk file path or null
     */
    public static resolvePackageLpk(packageName: string, projectDir: string): string | null {
        // 1. Check project directory
        let lpkPath = path.join(projectDir, packageName + '.lpk');
        if (fs.existsSync(lpkPath)) return lpkPath;

        // 2. Check sibling directories
        const parentDir = path.dirname(projectDir);
        try {
            const dirs = fs.readdirSync(parentDir, { withFileTypes: true });
            for (const dir of dirs) {
                if (dir.isDirectory()) {
                    const candidate = path.join(parentDir, dir.name, packageName + '.lpk');
                    if (fs.existsSync(candidate)) return candidate;
                }
            }
        } catch (e) {}

        // 3. Search up to 2 levels in parent directory for more flexibility
        try {
            const grandParentDir = path.dirname(parentDir);
            const dirs = fs.readdirSync(grandParentDir, { withFileTypes: true });
            for (const dir of dirs) {
                if (dir.isDirectory()) {
                     const candidate = path.join(grandParentDir, dir.name, packageName + '.lpk');
                     if (fs.existsSync(candidate)) return candidate;
                     
                     // Deep check one more level
                     try {
                        const subDirs = fs.readdirSync(candidate.replace(packageName + '.lpk', ''), { withFileTypes: true });
                        for(const sub of subDirs) {
                            if(sub.isDirectory()) {
                                const deepCandidate = path.join(grandParentDir, dir.name, sub.name, packageName + '.lpk');
                                if (fs.existsSync(deepCandidate)) return deepCandidate;
                            }
                        }
                     }catch(ee){}
                }
            }
        } catch (e) {}

        return null;
    }

    /**
     * Parse .lpk file and extract UsageOptions (UnitPath and IncludeFiles)
     * @param lpkFilePath Path to .lpk file
     * @returns Object containing unitPaths and includePaths
     */
    public static parseLpkUsageOptions(lpkFilePath: string): { unitPaths: string[], includePaths: string[] } {
        const result: { unitPaths: string[], includePaths: string[] } = { unitPaths: [], includePaths: [] };
        try {
            const content = fs.readFileSync(lpkFilePath, 'utf8');
            const lpkDir = path.dirname(lpkFilePath);

            // 1. Extract PkgOutDir (from CompilerOptions -> UnitOutputDirectory)
            let pkgOutDir = "lib/$(TargetCPU)-$(TargetOS)"; // default Lazarus convention
            const outDirMatch = content.match(/<UnitOutputDirectory[^>]*Value=["']([^"']*)["']/i);
            if (outDirMatch && outDirMatch[1]) {
                pkgOutDir = outDirMatch[1];
            }

            // 2. Extract UsageOptions section
            const usageOptionsMatch = content.match(/<UsageOptions[^>]*>([\s\S]*?)<\/UsageOptions>/i);
            if (usageOptionsMatch) {
                const usageSection = usageOptionsMatch[1];

                const resolveVars = (p: string) => {
                    // Substitute Lazarus variables with absolute paths or VS Code variables
                    let substituted = p.replace(/\$\(PkgDir\)/g, lpkDir);
                    substituted = substituted.replace(/\$\(PkgOutDir\)/g, pkgOutDir);
                    
                    // Convert Lazarus $(Var) to VS Code ${var} format for CPU/OS
                    substituted = substituted.replace(/\$\(TargetCPU\)/g, "${targetCPU}");
                    substituted = substituted.replace(/\$\(TargetOS\)/g, "${targetOS}");

                    if (path.isAbsolute(substituted) || substituted.startsWith('${')) {
                        return substituted;
                    }
                    return path.resolve(lpkDir, substituted);
                };

                // Extract UnitPath
                const unitPathMatch = usageSection.match(/<UnitPath[^>]*Value=["']([^"']*)["']/i);
                if (unitPathMatch && unitPathMatch[1]) {
                    const rawPaths = unitPathMatch[1].split(';').filter(Boolean);
                    result.unitPaths = rawPaths.map(resolveVars);
                }

                // Extract IncludeFiles
                const includePathsMatch = usageSection.match(/<IncludeFiles[^>]*Value=["']([^"']*)["']/i);
                if (includePathsMatch && includePathsMatch[1]) {
                    const rawPaths = includePathsMatch[1].split(';').filter(Boolean);
                    result.includePaths = rawPaths.map(resolveVars);
                }
                
                // Also add the package source directory itself as a fallback search path
                if (!result.unitPaths.includes(lpkDir)) {
                    result.unitPaths.unshift(lpkDir);
                }
            }
        } catch (error) {
            console.error(`Error parsing .lpk file ${lpkFilePath}:`, error);
        }
        return result;
    }
}

export class LazarusProjectParser {
    /**
     * Parse Lazarus project file (.lpi) and extract project information
     * @param lpiFilePath Path to the .lpi file
     * @returns Project interface object
     */
    public static parseLpiFile(lpiFilePath: string): IProjectIntf {
        try {
            // Check if file exists
            if (!fs.existsSync(lpiFilePath)) {
                LazarusUtils.logWarning(`Lazarus project file does not exist: ${lpiFilePath}`);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            // Read .lpi file content and handle errors
            let lpiContent: string;
            try {
                lpiContent = fs.readFileSync(lpiFilePath, 'utf8');

                // Validate XML content
                if (!lpiContent || lpiContent.trim() === '') {
                    throw new Error('Empty file content');
                }
            } catch (readError) {
                LazarusUtils.logError(`Error reading Lazarus project file: ${lpiFilePath}`, readError);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            // Check for corresponding .lps file (Session file)
            const lpsFilePath = lpiFilePath.replace(/\.lpi$/i, '.lps');
            let lpsContent: string | null = null;
            
            if (fs.existsSync(lpsFilePath)) {
                try {
                    lpsContent = fs.readFileSync(lpsFilePath, 'utf8');
                    LazarusUtils.logInfo(`Found corresponding .lps file: ${lpsFilePath}`);
                } catch (lpsError) {
                    LazarusUtils.logWarning(`Error reading .lps file: ${lpsFilePath}`);
                    lpsContent = null;
                }
            }

            // Parse XML using fast-xml-parser
            const parser = new XMLParser({
                ignoreAttributes: false,
                attributeNamePrefix: '@_',
                allowBooleanAttributes: true,
                parseAttributeValue: true,
                trimValues: true
            });

            let lpiXmlObj;
            try {
                lpiXmlObj = parser.parse(lpiContent);
            } catch (parseError) {
                LazarusUtils.logError(`Error parsing XML in Lazarus project file: ${lpiFilePath}`, parseError);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            // Parse .lps file (if exists)
            let lpsXmlObj = null;
            if (lpsContent) {
                try {
                    lpsXmlObj = parser.parse(lpsContent);
                } catch (lpsParseError) {
                    LazarusUtils.logWarning(`Error parsing XML in .lps file: ${lpsFilePath}`);
                    lpsXmlObj = null;
                }
            }

            // Navigate to CONFIG section
            if (!lpiXmlObj.CONFIG) {
                LazarusUtils.logWarning(`Invalid Lazarus project file: ${lpiFilePath} - missing CONFIG section`);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            const config = lpiXmlObj.CONFIG;

            // Extract project title
            let projectTitle = path.basename(lpiFilePath, '.lpi');
            if (config.ProjectOptions?.Title?.['@_Value']) {
                projectTitle = config.ProjectOptions.Title['@_Value'];
            }

            // Extract main file
            let mainFile = '';
            let mainUnitIndex = -1;

            // Try to get main unit index
            if (config.ProjectOptions?.MainUnit?.['@_Value'] !== undefined) {
                mainUnitIndex = parseInt(config.ProjectOptions.MainUnit['@_Value']);
            }

            // Find corresponding unit file
            if (config.ProjectOptions?.Units) {
                const units = config.ProjectOptions.Units;

                // Handle different XML structures of Units
                if (mainUnitIndex >= 0) {
                    // Try index access first
                    if (units[`Unit${mainUnitIndex}`]?.Filename?.['@_Value']) {
                        mainFile = units[`Unit${mainUnitIndex}`].Filename['@_Value'];
                    } else if (Array.isArray(units.Unit)) {
                        // If Units.Unit is an array, find the unit with the correct index
                        for (const unit of units.Unit) {
                            if (unit['@_UnitName'] === `Unit${mainUnitIndex}` && unit.Filename?.['@_Value']) {
                                mainFile = unit.Filename['@_Value'];
                                break;
                            }
                        }
                    }
                }

                // If still not found, look for .lpr file in Units.Unit array
                if (!mainFile && units) {
                    if(Array.isArray(units.Unit)) {
                        // Array form of Units
                        for (const unit of units.Unit) {
                            if (unit.Filename?.['@_Value']?.endsWith('.lpr') || unit.Filename?.['@_Value']?.endsWith('.dpr')) {
                                mainFile = unit.Filename['@_Value'];
                                break;
                            }
                        }
                    }else if (units['@_Count']!==undefined) {
                        let cnt=units['@_Count'];

                        for (let i = 0; i < cnt; i++) {
                            const unit = units[`Unit${i}`];
                            if (unit.Filename?.['@_Value']?.endsWith('.lpr')|| unit.Filename?.['@_Value']?.endsWith('.dpr')) {
                                mainFile = unit.Filename['@_Value'];
                                break;
                            }
                        }
                    } else if (units.Unit) {
                        // Single unit case
                        mainFile = units.Unit.Filename['@_Value'];
                    } 
                    if(!mainFile) {
                        mainFile = projectTitle + '.lpr'; // Use project title as main file by default
                    }
                }
            }

            // Get active mode (if specified)
            let activeMode: string | undefined;
            if (config.ProjectOptions?.ActiveMode?.['@_Value']) {
                activeMode = config.ProjectOptions.ActiveMode['@_Value'];
            }

            // Create main project info object
            const projectInfo = new LazarusProject(
                projectTitle,
                mainFile,
                lpiFilePath,
                false // Default status will be determined later
            );

            // Extract required packages
            this.extractRequiredPackages(config, projectInfo);

            // Extract build modes (merge build modes from .lpi and .lps files)
            let foundBuildModes = false;
            let skippedBuildModes = 0;

            // Get build modes from both files
            const allBuildModes = this.extractBuildModesFromBothFiles(config, lpsXmlObj);
            let useLCL=projectInfo.requiredPackages.includes('LCL');

            if (allBuildModes.length > 0) {
                foundBuildModes = true;
                LazarusUtils.logInfo(`Found ${allBuildModes.length} build modes from .lpi and .lps files`);

                // Process each build mode
                for (const mode of allBuildModes) {
                    foundBuildModes = true;

                    // Extract mode name and ID
                    let modeName = '';
                    let modeId = '';
                    let isDefault = false;

                    // Handle different XML structures for Name and Identifier
                    if (mode['@_Name']) {
                        modeName = mode['@_Name'];
                    } else if (mode.Name?.['@_Value']) {
                        modeName = mode.Name['@_Value'];
                    } else if (mode.Name?.Value) {
                        modeName = mode.Name.Value;
                    }

                    // Check if this is the default mode
                    if (mode['@_Default'] === 'True' || mode['@_Default'] === true) {
                        isDefault = true;
                    }

                    if (mode.Identifier?.['@_Value']) {
                        modeId = mode.Identifier['@_Value'];
                    } else if (mode.Identifier?.Value) {
                        modeId = mode.Identifier.Value;
                    } else if (modeName) {
                        // If not specified, generate ID from name
                        modeId = modeName.replace(/[^a-zA-Z0-9]/g, '_').toLowerCase();
                    }

                    // Skip if no valid name
                    if (!modeName) {
                        LazarusUtils.logWarning(`Skipping build mode with missing name in ${lpiFilePath}`);
                        continue;
                    }

                    // Choose correct content based on build mode source
                    const sourceContent = mode._sourceFile === 'lps' ? lpsContent : lpiContent;
                    LazarusUtils.logInfo(`Processing build mode "${modeName}" from .${mode._sourceFile} file`);
                    
                    // Extract target platform for this build mode
                    const targetPlatform = LazarusUtils.extractTargetPlatformForBuildMode(sourceContent || lpiContent, modeName);

                    // Extract search paths for this build mode
                    const searchPaths = this.extractSearchPathsForBuildMode(sourceContent || lpiContent, modeName);

                    // Create LazarusBuildMode object for this build mode
                    const buildMode = new LazarusBuildModeTask(
                        modeName,
                        false, // Will be determined later based on priority rules
                        mode._sourceFile === 'lpi',
                        projectInfo,
                        modeName,
                        targetPlatform.targetOS,
                        targetPlatform.targetCPU,
                        targetPlatform.targetFile // Get target file name from build mode's compiler options
                    );

                    // Set additional properties
                    buildMode.targetOS = targetPlatform.targetOS;
                    buildMode.targetCPU = targetPlatform.targetCPU;
                    buildMode.unitPaths = searchPaths.unitPaths;
                    buildMode.includePaths = searchPaths.includePaths;
                    buildMode.libraryPaths = searchPaths.libraryPaths;
                    buildMode.outputDirectory = searchPaths.outputDirectory;
                    buildMode.objectPath = searchPaths.objectPath;
                    buildMode.useLCL = useLCL;
                    // Extract compiler options for this build mode (using correct source content)
                    this.extractDetailedBuildOptionsForBuildMode(buildMode, sourceContent || lpiContent);

                    // Store build mode source info
                    (buildMode as any)._sourceFile = mode._sourceFile;

            

                    // Store additional metadata for default determination
                    (buildMode as any)._isLazarusDefault = isDefault;
                    (buildMode as any)._isActiveMode = activeMode === modeId;

                    // Add build mode to project's task list
                    projectInfo.tasks.push(buildMode);
                }
            }

            // If no build modes found or all skipped, create a default one
            if (projectInfo.tasks.length === 0) {
                if (foundBuildModes) {
                    LazarusUtils.logWarning(`All build modes in ${lpiFilePath} were skipped because they don't have compiler options`);
                } else {
                    LazarusUtils.logWarning(`No build modes found in ${lpiFilePath}, creating default`);
                }

                // Get system defaults
                const systemDefaults = LazarusUtils.getSystemDefaults();
                
                // Create default build mode
                const defaultBuildMode = new LazarusBuildModeTask(
                    'Default',
                    true, // This is the default task
                    true,
                    projectInfo,
                    'Default',
                    systemDefaults.targetOS,
                    systemDefaults.targetCPU,
                    undefined // No explicit target file name for default build mode
                );
                defaultBuildMode.targetOS = systemDefaults.targetOS;
                defaultBuildMode.targetCPU = systemDefaults.targetCPU;
                defaultBuildMode.compilerOptions = ['-B']; // Basic force rebuild option

                // Add default build mode to project's task list
                projectInfo.tasks.push(defaultBuildMode);
            }

            // Log summary of build mode processing
            if (foundBuildModes) {
                if (skippedBuildModes > 0) {
                    LazarusUtils.logInfo(`Processed ${projectInfo.tasks.length + skippedBuildModes} build modes in ${lpiFilePath}, skipped ${skippedBuildModes} modes without compiler options`);
                } else {
                    LazarusUtils.logInfo(`Processed ${projectInfo.tasks.length} build modes in ${lpiFilePath}`);
                }
            }

            return projectInfo;
        } catch (error) {
            LazarusUtils.logError(`Error parsing Lazarus project file: ${lpiFilePath}`, error);
            return this.createDefaultProjectInfo(lpiFilePath);
        }
    }

    /**
     * 从 .lpi 和 .lps 文件中提取所有构建模式
     * @param lpiConfig LPI 文件的配置对象
     * @param lpsXmlObj LPS 文件的 XML 对象（可选）
     * @returns 所有构建模式的数组，每个模式都标记了来源
     */
    private static extractBuildModesFromBothFiles(lpiConfig: any, lpsXmlObj: any): any[] {
        let allBuildModes: any[] = [];
        const processedModeNames = new Set<string>();

        // 从 .lpi 文件中获取构建模式
        if (lpiConfig.ProjectOptions?.BuildModes) {
            const lpiBuildModes = this.extractBuildModesFromConfig(lpiConfig.ProjectOptions.BuildModes);
            for (const mode of lpiBuildModes) {
                const modeName = this.extractModeName(mode);
                if (modeName && !processedModeNames.has(modeName)) {
                    // 标记构建模式来源
                    mode._sourceFile = 'lpi';
                    allBuildModes.push(mode);
                    processedModeNames.add(modeName);
                    LazarusUtils.logInfo(`Found build mode "${modeName}" from .lpi file`);
                }
            }
        }

        // 从 .lps 文件中获取构建模式（如果存在）
        if (lpsXmlObj && lpsXmlObj.CONFIG?.ProjectSession?.BuildModes) {
            const lpsBuildModes = this.extractBuildModesFromConfig(lpsXmlObj.CONFIG.ProjectSession.BuildModes);
            for (const mode of lpsBuildModes) {
                const modeName = this.extractModeName(mode);
                if (modeName && !processedModeNames.has(modeName)) {
                    // 标记构建模式来源
                    mode._sourceFile = 'lps';
                    allBuildModes.push(mode);
                    processedModeNames.add(modeName);
                    LazarusUtils.logInfo(`Found build mode "${modeName}" from .lps file`);
                }
            }
        }

        return allBuildModes;
    }

    /**
     * 从配置对象中提取构建模式列表
     * @param buildModesConfig 构建模式配置对象
     * @returns 构建模式数组
     */
    private static extractBuildModesFromConfig(buildModesConfig: any): any[] {
        let buildModesList: any[] = [];

        // 检查 BuildMode 结构（较新的 Lazarus 版本）
        if (buildModesConfig.BuildMode) {
            if (Array.isArray(buildModesConfig.BuildMode)) {
                buildModesList = buildModesConfig.BuildMode;
            } else {
                buildModesList = [buildModesConfig.BuildMode];
            }
        }
        // 检查 Item 结构（在许多 LPI 文件中常见）
        else if (buildModesConfig.Item) {
            if (Array.isArray(buildModesConfig.Item)) {
                buildModesList = buildModesConfig.Item;
            } else {
                buildModesList = [buildModesConfig.Item];
            }
        }
        // 检查编号的 Item 结构（如 Item1, Item2 等）
        else {
            // 查找所有编号的 Item 属性
            const keys = Object.keys(buildModesConfig);
            const itemKeys = keys.filter(key => /^Item\d*$/i.test(key));
            
            if (itemKeys.length > 0) {
                // 按数字排序（如果有数字的话）
                itemKeys.sort((a, b) => {
                    const numA = parseInt(a.replace(/Item/i, '')) || 0;
                    const numB = parseInt(b.replace(/Item/i, '')) || 0;
                    return numA - numB;
                });
                
                for (const key of itemKeys) {
                    const item = buildModesConfig[key];
                    if (item) {
                        buildModesList.push(item);
                    }
                }
            }
        }

        return buildModesList;
    }

    /**
     * 从构建模式对象中提取模式名称
     * @param mode 构建模式对象
     * @returns 模式名称，如果未找到则返回空字符串
     */
    private static extractModeName(mode: any): string {
        let modeName = '';

        // 处理 Name 和 Identifier 的不同 XML 结构
        if (mode['@_Name']) {
            modeName = mode['@_Name'];
        } else if (mode.Name?.['@_Value']) {
            modeName = mode.Name['@_Value'];
        } else if (mode.Name?.Value) {
            modeName = mode.Name.Value;
        }

        return modeName;
    }

    /**
     * 为 Lazarus 项目文件创建默认项目信息
     * @param lpiFilePath .lpi 文件的路径
     * @returns 默认项目信息
     */
    public static createDefaultProjectInfo(lpiFilePath: string): LazarusProject {
        const fileName = path.basename(lpiFilePath);
        const projectTitle = path.basename(lpiFilePath, '.lpi');

        // 创建默认项目信息
        const projectInfo = new LazarusProject(
            projectTitle,
            '', // 没有主文件
            lpiFilePath,
            true // 是默认
        );

        // 获取系统默认值
        const systemDefaults = LazarusUtils.getSystemDefaults();
        
        // 创建此构建模式的默认构建模式
        const defaultBuildMode = new LazarusBuildModeTask(
            'Default',
            true, // 这是默认任务
            true,
            projectInfo,
            'Default',
            systemDefaults.targetOS,
            systemDefaults.targetCPU,
            undefined // 对于默认项目信息，没有明确的目标文件名
        );
        defaultBuildMode.targetOS = systemDefaults.targetOS;
        defaultBuildMode.targetCPU = systemDefaults.targetCPU;
        defaultBuildMode.compilerOptions = ['-B']; // 基本的强制重建选项

        // 将默认构建模式添加到项目的任务列表
        projectInfo.tasks.push(defaultBuildMode);

        return projectInfo;
    }


    private static extractDetailedBuildOptionsForBuildMode(buildMode: LazarusBuildModeTask, content: string){
        // 初始化详细构建选项对象
        buildMode.detailedBuildOptions = {};

        try {
            // 获取此构建模式的编译器部分
            const compilerSection = LazarusUtils.getCompilerSectionForBuildMode(content, buildMode.buildMode || '');

            if (compilerSection) {
                // === 调试选项 ===
                const debugInfoTypeMatch = compilerSection.match(/<DebugInfoType[^>]*Value=["']([^"']+)["']/i);
                if (debugInfoTypeMatch) {
                    buildMode.detailedBuildOptions.debugInfoType = debugInfoTypeMatch[1];
                }
                
                const useHeaptrcMatch = compilerSection.match(/<UseHeaptrc[^>]*Value=["'](True|False)["']/i);
                if (useHeaptrcMatch) {
                    buildMode.detailedBuildOptions.useHeaptrc = useHeaptrcMatch[1].toLowerCase() === 'true';
                }
                
                const trashVariablesMatch = compilerSection.match(/<TrashVariables[^>]*Value=["'](True|False)["']/i);
                if (trashVariablesMatch) {
                    buildMode.detailedBuildOptions.trashVariables = trashVariablesMatch[1].toLowerCase() === 'true';
                }
                
                const useExternalDbgSymsMatch = compilerSection.match(/<UseExternalDbgSyms[^>]*Value=["'](True|False)["']/i);
                if (useExternalDbgSymsMatch) {
                    buildMode.detailedBuildOptions.useExternalDbgSyms = useExternalDbgSymsMatch[1].toLowerCase() === 'true';
                }
                
                const generateDebugInfoMatch = compilerSection.match(/<GenerateDebugInfo[^>]*Value=["'](True|False)["']/i);
                if (generateDebugInfoMatch) {
                    buildMode.detailedBuildOptions.generateDebugInfo = generateDebugInfoMatch[1].toLowerCase() === 'true';
                }
                
                // === 代码生成选项 ===
                const ioChecksMatch = compilerSection.match(/<IOChecks[^>]*Value=["'](True|False)["']/i);
                if (ioChecksMatch) {
                    buildMode.detailedBuildOptions.ioChecks = ioChecksMatch[1].toLowerCase() === 'true';
                }
                
                const rangeChecksMatch = compilerSection.match(/<RangeChecks[^>]*Value=["'](True|False)["']/i);
                if (rangeChecksMatch) {
                    buildMode.detailedBuildOptions.rangeChecks = rangeChecksMatch[1].toLowerCase() === 'true';
                }
                
                const overflowChecksMatch = compilerSection.match(/<OverflowChecks[^>]*Value=["'](True|False)["']/i);
                if (overflowChecksMatch) {
                    buildMode.detailedBuildOptions.overflowChecks = overflowChecksMatch[1].toLowerCase() === 'true';
                }
                
                const stackChecksMatch = compilerSection.match(/<StackChecks[^>]*Value=["'](True|False)["']/i);
                if (stackChecksMatch) {
                    buildMode.detailedBuildOptions.stackChecks = stackChecksMatch[1].toLowerCase() === 'true';
                }
                
                const smartLinkUnitMatch = compilerSection.match(/<SmartLinkUnit[^>]*Value=["'](True|False)["']/i);
                if (smartLinkUnitMatch) {
                    buildMode.detailedBuildOptions.smartLinkUnit = smartLinkUnitMatch[1].toLowerCase() === 'true';
                }
                
                const linkSmartMatch = compilerSection.match(/<LinkSmart[^>]*Value=["'](True|False)["']/i);
                if (linkSmartMatch) {
                    buildMode.detailedBuildOptions.linkSmart = linkSmartMatch[1].toLowerCase() === 'true';
                }
                
                const verifyObjMethodMatch = compilerSection.match(/<VerifyObjMethodCallValidity[^>]*Value=["'](True|False)["']/i);
                if (verifyObjMethodMatch) {
                    buildMode.detailedBuildOptions.verifyObjMethodCallValidity = verifyObjMethodMatch[1].toLowerCase() === 'true';
                }
                
                const optimizationMatch = compilerSection.match(/<OptimizationLevel[^>]*Value=["']([^"']+)["']/i);
                if (optimizationMatch) {
                    buildMode.detailedBuildOptions.optimizationLevel = parseInt(optimizationMatch[1]);
                }
                
                // === 解析选项 ===
                const includeAssertionMatch = compilerSection.match(/<IncludeAssertionCode[^>]*Value=["'](True|False)["']/i);
                if (includeAssertionMatch) {
                    buildMode.detailedBuildOptions.includeAssertionCode = includeAssertionMatch[1].toLowerCase() === 'true';
                }
                
                // === 自定义选项 ===
                const otherDefinesMatch = compilerSection.match(/<OtherDefines[^>]*Value=["']([^"']*)["']/i);
                if (otherDefinesMatch && otherDefinesMatch[1]) {
                    const defines = otherDefinesMatch[1].split(/[\s;]+/).filter(Boolean);
                    buildMode.detailedBuildOptions.customDefines = defines;
                }
                
                // 条件定义等其他可能的定义
                const conditionalDefinesMatch = compilerSection.match(/<ConditionalDefines[^>]*Value=["']([^"']*)["']/i);
                if (conditionalDefinesMatch && conditionalDefinesMatch[1]) {
                    const defines = conditionalDefinesMatch[1].split(/[\s;]+/).filter(Boolean);
                    buildMode.detailedBuildOptions.conditionalDefines = defines;
                }
                
                // 自定义选项
                const customOptionsMatch = compilerSection.match(/<CustomOptions[^>]*Value=["']([^"']*)["']/i);
                if (customOptionsMatch && customOptionsMatch[1]) {
                    const extraOptions = customOptionsMatch[1].split(/\s+/).filter(Boolean);
                    buildMode.detailedBuildOptions.customOptions = extraOptions;
                }
                
                const SyntaxMode = compilerSection.match(/<SyntaxMode[^>]*Value=["']([^"']+)["']/i);
                if (SyntaxMode && SyntaxMode[1]) {
                    buildMode.detailedBuildOptions.syntaxMode = SyntaxMode[1];
                }
            }
            
        } catch (error) {
            console.error(`Error extracting detailed build options for build mode ${buildMode.buildMode}:`, error);
        }
    }

    /**
     * 提取特定构建模式的搜索路径
     * @param content LPI 文件内容
     * @param buildModeName 构建模式名称
     * @returns 包含搜索路径的对象
     */
    private static extractSearchPathsForBuildMode(content: string, buildModeName: string): {
        unitPaths: string[],
        includePaths: string[],
        libraryPaths: string[],
        outputDirectory?: string,
        objectPath?: string
    } {
        const result: {
            unitPaths: string[],
            includePaths: string[],
            libraryPaths: string[],
            outputDirectory?: string,
            objectPath?: string
        } = {
            unitPaths: [],
            includePaths: [],
            libraryPaths: []
        };

        try {
            // 获取此构建模式的编译器部分
            const compilerSection = LazarusUtils.getCompilerSectionForBuildMode(content, buildModeName);

            if (compilerSection) {
                // 提取单元路径
                const unitPathsMatch = compilerSection.match(/<OtherUnitFiles[^>]*Value=["']([^"']*)["']/i);
                if (unitPathsMatch && unitPathsMatch[1]) {
                    result.unitPaths = unitPathsMatch[1].split(';').filter(Boolean);
                }

                // 提取包含路径
                const includePathsMatch = compilerSection.match(/<IncludeFiles[^>]*Value=["']([^"']*)["']/i);
                if (includePathsMatch && includePathsMatch[1]) {
                    result.includePaths = includePathsMatch[1].split(';').filter(Boolean);
                }

                // 提取库路径
                const libraryPathsMatch = compilerSection.match(/<Libraries[^>]*Value=["']([^"']*)["']/i);
                if (libraryPathsMatch && libraryPathsMatch[1]) {
                    result.libraryPaths = libraryPathsMatch[1].split(';').filter(Boolean);
                }

                // 提取输出目录
                const outputDirMatch = compilerSection.match(/<UnitOutputDirectory[^>]*Value=["']([^"']*)["']/i);
                if (outputDirMatch && outputDirMatch[1]) {
                    result.outputDirectory = outputDirMatch[1];
                }

                // 提取对象路径
                const objectPathMatch = compilerSection.match(/<ObjectPath[^>]*Value=["']([^"']*)["']/i);
                if (objectPathMatch && objectPathMatch[1]) {
                    result.objectPath = objectPathMatch[1];
                }
            }
        } catch (error) {
            console.error(`Error extracting search paths for build mode ${buildModeName}:`, error);
        }

        return result;
    }

    /**
     * 提取项目所需的包（RequiredPackages）
     * @param config 项目配置对象
     * @param projectInfo 项目信息对象
     */
    private static extractRequiredPackages(config: any, projectInfo: LazarusProject): void {
        try {
            // 检查是否存在 RequiredPackages 部分
            if (config.ProjectOptions?.RequiredPackages) {
                const requiredPackages = config.ProjectOptions.RequiredPackages;
                
                // 获取包的数量
                let packageCount = 0;
                if (requiredPackages['@_Count'] !== undefined) {
                    packageCount = parseInt(requiredPackages['@_Count'].toString());
                }

                // 提取包名称
                const packageNames: string[] = [];

                if (packageCount > 0) {
                    // 处理编号的包项目（如 Item1, Item2 等）
                    for (let i = 1; i <= packageCount; i++) {
                        const itemKey = `Item${i}`;
                        const packageItem = requiredPackages[itemKey];
                        
                        if (packageItem) {
                            let packageName = '';
                            
                            // 尝试多种方式提取包名称
                            if (packageItem.PackageName?.['@_Value']) {
                                packageName = packageItem.PackageName['@_Value'];
                            } else if (packageItem.PackageName?.Value) {
                                packageName = packageItem.PackageName.Value;
                            } else if (packageItem['@_PackageName']) {
                                packageName = packageItem['@_PackageName'];
                            }
                            
                            if (packageName && !packageNames.includes(packageName)) {
                                packageNames.push(packageName);
                                LazarusUtils.logInfo(`Found required package: ${packageName}`);
                            }
                        }
                    }
                }

                // 也检查是否有 Item 数组形式的包
                if (requiredPackages.Item) {
                    const items = Array.isArray(requiredPackages.Item) ? requiredPackages.Item : [requiredPackages.Item];
                    
                    for (const item of items) {
                        let packageName = '';
                        
                        if (item.PackageName?.['@_Value']) {
                            packageName = item.PackageName['@_Value'];
                        } else if (item.PackageName?.Value) {
                            packageName = item.PackageName.Value;
                        } else if (item['@_PackageName']) {
                            packageName = item['@_PackageName'];
                        }
                        
                        if (packageName && !packageNames.includes(packageName)) {
                            packageNames.push(packageName);
                            LazarusUtils.logInfo(`Found required package: ${packageName}`);
                        }
                    }
                }

                // 将提取的包名称存储到项目信息中
                projectInfo.requiredPackages = packageNames;
                
                if (packageNames.length > 0) {
                    LazarusUtils.logInfo(`Total required packages found: ${packageNames.length} - ${packageNames.join(', ')}`);
                    
                    // 为自定义包解析 LPK 文件并注入搜索路径
                    const projectDir = path.dirname(projectInfo.file);
                    for (const pkgName of packageNames) {
                        // 跳过内置的标准包（启发式判断，通常首字母大写且较短或众所周知）
                        if (['LCL', 'FCL', 'LCLBase', 'LazUtils', 'Codetools'].includes(pkgName)) continue;
                        
                        const lpkPath = LazarusUtils.resolvePackageLpk(pkgName, projectDir);
                        if (lpkPath) {
                            LazarusUtils.logInfo(`Resolved .lpk for package ${pkgName}: ${lpkPath}`);
                            const usage = LazarusUtils.parseLpkUsageOptions(lpkPath);
                            
                            // 合并路径
                            usage.unitPaths.forEach(p => {
                                if (!projectInfo.packageSearchPaths.includes(p)) {
                                    projectInfo.packageSearchPaths.push(p);
                                }
                            });
                            usage.includePaths.forEach(p => {
                                if (!projectInfo.packageIncludePaths.includes(p)) {
                                    projectInfo.packageIncludePaths.push(p);
                                }
                            });
                        }
                    }
                }
            }
        } catch (error) {
            LazarusUtils.logError('Error extracting required packages', error);
            // 确保即使出错也有一个空数组
            projectInfo.requiredPackages = [];
        }
    }
    /**
     * Parse Lazarus package file (.lpk) and extract project information
     * @param lpkFilePath Path to the .lpk file
     * @returns Project interface object
     */
    public static parseLpkFile(lpkFilePath: string): IProjectIntf {
        try {
            // Check if file exists
            if (!fs.existsSync(lpkFilePath)) {
                return this.createDefaultProjectInfo(lpkFilePath);
            }

            // Read .lpk file content
            const lpkContent = fs.readFileSync(lpkFilePath, 'utf8');
            const projectTitle = path.basename(lpkFilePath, '.lpk');
            
            // Create XML parser
            const parser = new XMLParser({
                ignoreAttributes: false,
                attributeNamePrefix: '@_',
                allowBooleanAttributes: true,
                parseAttributeValue: true,
                trimValues: true
            });

            let lpkXmlObj;
            try {
                lpkXmlObj = parser.parse(lpkContent);
            } catch (parseError) {
                LazarusUtils.logError(`Error parsing XML in Lazarus package file: ${lpkFilePath}`, parseError);
                return this.createDefaultProjectInfo(lpkFilePath);
            }

            const packageConfig = lpkXmlObj.CONFIG || lpkXmlObj.Package;
            
            // Create main project info object
            const projectInfo = new LazarusProject(
                projectTitle,
                '', // Packages don't have a single main .lpr file
                lpkFilePath,
                false
            );

            if (packageConfig) {
                // Extract required packages
                this.extractRequiredPackages(packageConfig, projectInfo);
            }

            // Get system defaults
            const systemDefaults = LazarusUtils.getSystemDefaults();
            
            // Create a default build mode for the package
            const buildMode = new LazarusBuildModeTask(
                'Compile Package',
                true, // Default task for the package
                true,
                projectInfo,
                'Default',
                systemDefaults.targetOS,
                systemDefaults.targetCPU
            );

            // Extract compiler options for the package
            // In .lpk, CompilerOptions is usually directly under Package
            const compilerOptionsMatch = lpkContent.match(/<CompilerOptions[^>]*>([\s\S]*?)<\/CompilerOptions>/i);
            if (compilerOptionsMatch) {
                // We use "" as buildModeName to trigger the global fallback in getCompilerSectionForBuildMode
                this.extractDetailedBuildOptionsForBuildMode(buildMode, lpkContent);
                
                // Extract search paths
                const searchPaths = this.extractSearchPathsForBuildMode(lpkContent, "");
                buildMode.unitPaths = searchPaths.unitPaths;
                buildMode.includePaths = searchPaths.includePaths;
                buildMode.libraryPaths = searchPaths.libraryPaths;
                buildMode.outputDirectory = searchPaths.outputDirectory;
                buildMode.objectPath = searchPaths.objectPath;
            }

            // Add task to project
            projectInfo.tasks.push(buildMode);

            return projectInfo;
        } catch (error) {
            LazarusUtils.logError(`Error parsing Lazarus package file: ${lpkFilePath}`, error);
            return this.createDefaultProjectInfo(lpkFilePath);
        }
    }
}