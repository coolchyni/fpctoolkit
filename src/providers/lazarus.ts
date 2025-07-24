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

            // 如果在构建模式特定部分中未找到，则查找全局编译器选项
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
     * 提取特定构建模式的目标平台信息和目标文件
     * @param content LPI 文件内容
     * @param buildModeName 构建模式名称
     * @returns 包含目标操作系统、CPU 和目标文件的对象
     */
    public static extractTargetPlatformForBuildMode(content: string, buildModeName: string): {
        targetOS?: string;
        targetCPU?: string;
        targetFile?: string;
    } {
        const result: { targetOS?: string; targetCPU?: string; targetFile?: string } = {};

        try {
            // 获取此构建模式的编译器部分
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
     * 获取目标操作系统和 CPU 的系统默认值
     * @returns 包含默认目标操作系统和 CPU 的对象
     */
    public static getSystemDefaults(): {
        targetOS: string,
        targetCPU: string,
        lazarusDir?: string,
        fpcVersion?: string
    } {
        return {
            targetOS: this.getDefaultTargetOS(),
            targetCPU: this.getDefaultTargetCPU(),
            lazarusDir: this.cachedSystemInfo.lazarusDir,
            fpcVersion: this.cachedSystemInfo.fpcVersion
        };
    }
}

export class LazarusProjectParser {
    /**
     * 解析 Lazarus 项目文件（.lpi）并提取项目信息
     * @param lpiFilePath .lpi 文件的路径
     * @returns 项目接口对象
     */
    public static parseLpiFile(lpiFilePath: string): IProjectIntf {
        try {
            // 检查文件是否存在
            if (!fs.existsSync(lpiFilePath)) {
                LazarusUtils.logWarning(`Lazarus project file does not exist: ${lpiFilePath}`);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            // 读取 .lpi 文件内容并处理错误
            let lpiContent: string;
            try {
                lpiContent = fs.readFileSync(lpiFilePath, 'utf8');

                // 验证 XML 内容
                if (!lpiContent || lpiContent.trim() === '') {
                    throw new Error('Empty file content');
                }
            } catch (readError) {
                LazarusUtils.logError(`Error reading Lazarus project file: ${lpiFilePath}`, readError);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            // 检查同名的 .lps 文件（Session file）
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

            // 使用 fast-xml-parser 解析 XML
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

            // 解析 .lps 文件（如果存在）
            let lpsXmlObj = null;
            if (lpsContent) {
                try {
                    lpsXmlObj = parser.parse(lpsContent);
                } catch (lpsParseError) {
                    LazarusUtils.logWarning(`Error parsing XML in .lps file: ${lpsFilePath}`);
                    lpsXmlObj = null;
                }
            }

            // 导航到 CONFIG 部分
            if (!lpiXmlObj.CONFIG) {
                LazarusUtils.logWarning(`Invalid Lazarus project file: ${lpiFilePath} - missing CONFIG section`);
                return this.createDefaultProjectInfo(lpiFilePath);
            }

            const config = lpiXmlObj.CONFIG;

            // 提取项目标题
            let projectTitle = path.basename(lpiFilePath, '.lpi');
            if (config.ProjectOptions?.Title?.['@_Value']) {
                projectTitle = config.ProjectOptions.Title['@_Value'];
            }

            // 提取主文件
            let mainFile = '';
            let mainUnitIndex = -1;

            // 尝试获取主单元索引
            if (config.ProjectOptions?.MainUnit?.['@_Value'] !== undefined) {
                mainUnitIndex = parseInt(config.ProjectOptions.MainUnit['@_Value']);
            }

            // 查找相应的单元文件
            if (config.ProjectOptions?.Units) {
                const units = config.ProjectOptions.Units;

                // 处理 Units 的不同 XML 结构
                if (mainUnitIndex >= 0) {
                    // 首先尝试索引访问
                    if (units[`Unit${mainUnitIndex}`]?.Filename?.['@_Value']) {
                        mainFile = units[`Unit${mainUnitIndex}`].Filename['@_Value'];
                    } else if (Array.isArray(units.Unit)) {
                        // 如果 Units.Unit 是数组，查找具有正确索引的单元
                        for (const unit of units.Unit) {
                            if (unit['@_UnitName'] === `Unit${mainUnitIndex}` && unit.Filename?.['@_Value']) {
                                mainFile = unit.Filename['@_Value'];
                                break;
                            }
                        }
                    }
                }

                // 如果仍未找到，在 Units.Unit 数组中查找 .lpr 文件
                if (!mainFile && units) {
                    if(Array.isArray(units.Unit)) {
                        // 数组形式的 Units
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
                        // 单个单元情况
                        mainFile = units.Unit.Filename['@_Value'];
                    } 
                    if(!mainFile) {
                        mainFile = projectTitle + '.lpr'; // 默认使用项目标题作为主文件
                    }
                }
            }

            // 获取活动模式（如果指定）
            let activeMode: string | undefined;
            if (config.ProjectOptions?.ActiveMode?.['@_Value']) {
                activeMode = config.ProjectOptions.ActiveMode['@_Value'];
            }

            // 创建主项目信息对象
            const projectInfo = new LazarusProject(
                projectTitle,
                mainFile,
                path.basename(lpiFilePath),
                false // 默认状态将在后面确定
            );

            // 提取所需包（RequiredPackages）
            this.extractRequiredPackages(config, projectInfo);

            // 提取构建模式（合并 .lpi 和 .lps 文件中的构建模式）
            let foundBuildModes = false;
            let skippedBuildModes = 0;

            // 从两个文件中获取构建模式
            const allBuildModes = this.extractBuildModesFromBothFiles(config, lpsXmlObj);
            let useLCL=projectInfo.requiredPackages.includes('LCL');

            if (allBuildModes.length > 0) {
                foundBuildModes = true;
                LazarusUtils.logInfo(`Found ${allBuildModes.length} build modes from .lpi and .lps files`);

                // 处理每个构建模式
                for (const mode of allBuildModes) {
                    foundBuildModes = true;

                    // 提取模式名称和 ID
                    let modeName = '';
                    let modeId = '';
                    let isDefault = false;

                    // 处理 Name 和 Identifier 的不同 XML 结构
                    if (mode['@_Name']) {
                        modeName = mode['@_Name'];
                    } else if (mode.Name?.['@_Value']) {
                        modeName = mode.Name['@_Value'];
                    } else if (mode.Name?.Value) {
                        modeName = mode.Name.Value;
                    }

                    // 检查这是否是默认模式
                    if (mode['@_Default'] === 'True' || mode['@_Default'] === true) {
                        isDefault = true;
                    }

                    if (mode.Identifier?.['@_Value']) {
                        modeId = mode.Identifier['@_Value'];
                    } else if (mode.Identifier?.Value) {
                        modeId = mode.Identifier.Value;
                    } else if (modeName) {
                        // 如果未指定，则从名称生成 ID
                        modeId = modeName.replace(/[^a-zA-Z0-9]/g, '_').toLowerCase();
                    }

                    // 如果没有有效的名称，则跳过
                    if (!modeName) {
                        LazarusUtils.logWarning(`Skipping build mode with missing name in ${lpiFilePath}`);
                        continue;
                    }

                    // 根据构建模式来源选择正确的内容
                    const sourceContent = mode._sourceFile === 'lps' ? lpsContent : lpiContent;
                    LazarusUtils.logInfo(`Processing build mode "${modeName}" from .${mode._sourceFile} file`);
                    
                    // 提取此构建模式的目标平台
                    const targetPlatform = LazarusUtils.extractTargetPlatformForBuildMode(sourceContent || lpiContent, modeName);

                    // 提取此构建模式的搜索路径
                    const searchPaths = this.extractSearchPathsForBuildMode(sourceContent || lpiContent, modeName);

                    // 创建此构建模式的 LazarusBuildMode 对象
                    const buildMode = new LazarusBuildModeTask(
                        modeName,
                        false, // 将在后面基于优先级规则确定
                        mode._sourceFile === 'lpi',
                        projectInfo,
                        modeName,
                        targetPlatform.targetOS,
                        targetPlatform.targetCPU,
                        targetPlatform.targetFile // 从构建模式的编译器选项中获取目标文件名
                    );

                    // 设置附加属性
                    buildMode.targetOS = targetPlatform.targetOS;
                    buildMode.targetCPU = targetPlatform.targetCPU;
                    buildMode.unitPaths = searchPaths.unitPaths;
                    buildMode.includePaths = searchPaths.includePaths;
                    buildMode.libraryPaths = searchPaths.libraryPaths;
                    buildMode.outputDirectory = searchPaths.outputDirectory;
                    buildMode.objectPath = searchPaths.objectPath;
                    buildMode.useLCL = useLCL;
                    // 提取此构建模式的编译器选项（使用正确的源内容）
                    this.extractDetailedBuildOptionsForBuildMode(buildMode, sourceContent || lpiContent);

                    // 存储构建模式来源信息
                    (buildMode as any)._sourceFile = mode._sourceFile;

            

                    // 存储用于默认确定的附加元数据
                    (buildMode as any)._isLazarusDefault = isDefault;
                    (buildMode as any)._isActiveMode = activeMode === modeId;

                    // 将构建模式添加到项目的任务列表
                    projectInfo.tasks.push(buildMode);
                }
            }

            // 如果没有找到构建模式或所有构建模式都被跳过，创建一个默认的
            if (projectInfo.tasks.length === 0) {
                if (foundBuildModes) {
                    LazarusUtils.logWarning(`All build modes in ${lpiFilePath} were skipped because they don't have compiler options`);
                } else {
                    LazarusUtils.logWarning(`No build modes found in ${lpiFilePath}, creating default`);
                }

                // 获取系统默认值
                const systemDefaults = LazarusUtils.getSystemDefaults();
                
                // 创建默认构建模式
                const defaultBuildMode = new LazarusBuildModeTask(
                    'Default',
                    true, // 这是默认任务
                    true,
                    projectInfo,
                    'Default',
                    systemDefaults.targetOS,
                    systemDefaults.targetCPU,
                    undefined // 默认构建模式没有明确的目标文件名
                );
                defaultBuildMode.targetOS = systemDefaults.targetOS;
                defaultBuildMode.targetCPU = systemDefaults.targetCPU;
                defaultBuildMode.compilerOptions = ['-B']; // 基本的强制重建选项

                // 将默认构建模式添加到项目的任务列表
                projectInfo.tasks.push(defaultBuildMode);
            }

            // 记录构建模式处理的摘要
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
            fileName,
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
                }
            }
        } catch (error) {
            LazarusUtils.logError('Error extracting required packages', error);
            // 确保即使出错也有一个空数组
            projectInfo.requiredPackages = [];
        }
    }

    
}