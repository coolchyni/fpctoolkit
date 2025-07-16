import * as fs from 'fs';
import * as path from 'path';

export interface LazarusProjectInfo {
    title: string;
    mainFile: string;
    targetFile?: string;
    compilerOptions?: string[];
    searchPaths?: string[];
    unitPaths?: string[];
}

export class LazarusProjectParser {
    /**
     * 解析Lazarus项目文件(.lpi)
     * @param lpiFilePath .lpi文件路径
     * @returns 项目信息
     */
    public static parseLpiFile(lpiFilePath: string): LazarusProjectInfo | null {
        try {
            if (!fs.existsSync(lpiFilePath)) {
                return null;
            }

            const content = fs.readFileSync(lpiFilePath, 'utf-8');
            const projectInfo: LazarusProjectInfo = {
                title: path.basename(lpiFilePath, '.lpi'),
                mainFile: ''
            };

            // 解析项目标题
            const titleMatch = content.match(/<Title[^>]*>([^<]+)<\/Title>/i);
            if (titleMatch) {
                projectInfo.title = titleMatch[1];
            }

            // 解析主程序文件
            const mainUnitMatch = content.match(/<MainUnit[^>]*Value="([^"]+)"/i);
            if (mainUnitMatch) {
                const mainUnitIndex = parseInt(mainUnitMatch[1]);
                // 查找对应的单元文件
                const unitRegex = new RegExp(`<Unit${mainUnitIndex}[^>]*>.*?<Filename[^>]*Value="([^"]+)"`, 'is');
                const unitMatch = content.match(unitRegex);
                if (unitMatch) {
                    projectInfo.mainFile = unitMatch[1];
                } else {
                    // 如果没找到，尝试查找第一个.lpr文件
                    const lprMatch = content.match(/<Filename[^>]*Value="([^"]*\.lpr)"/i);
                    if (lprMatch) {
                        projectInfo.mainFile = lprMatch[1];
                    }
                }
            }

            // 解析目标文件名
            const targetMatch = content.match(/<TargetFileName[^>]*Value="([^"]+)"/i);
            if (targetMatch) {
                projectInfo.targetFile = targetMatch[1];
            }

            // 解析编译器选项
            const compilerOptions: string[] = [];
            
            // 解析调试信息
            const debugInfoMatch = content.match(/<DebugInfoType[^>]*Value="([^"]+)"/i);
            if (debugInfoMatch && debugInfoMatch[1] !== 'dsDwarf2') {
                if (debugInfoMatch[1] === 'dsStabs') {
                    compilerOptions.push('-gs');
                } else if (debugInfoMatch[1] === 'dsDwarf2Set') {
                    compilerOptions.push('-gw2');
                }
            }

            // 解析优化级别
            const optimizationMatch = content.match(/<OptimizationLevel[^>]*Value="([^"]+)"/i);
            if (optimizationMatch) {
                const level = parseInt(optimizationMatch[1]);
                if (level > 0) {
                    compilerOptions.push(`-O${level}`);
                }
            }

            // 解析自定义选项
            const customOptionsRegex = /<CustomOptions[^>]*>[\s\S]*?<Value>([^<]*)<\/Value>/;
            const customOptionsMatch = content.match(customOptionsRegex);
            if (customOptionsMatch) {
                const customOpts = customOptionsMatch[1].trim();
                if (customOpts) {
                    compilerOptions.push(...customOpts.split(/\s+/));
                }
            }

            projectInfo.compilerOptions = compilerOptions;

            // 解析搜索路径
            const searchPaths: string[] = [];
            const unitPaths: string[] = [];

            // 解析IncludePath
            const includePathRegex = /<IncludePath[^>]*>[\s\S]*?<Value>([^<]*)<\/Value>/;
            const includePathMatch = content.match(includePathRegex);
            if (includePathMatch) {
                const paths = includePathMatch[1].split(';').filter(p => p.trim());
                searchPaths.push(...paths);
            }

            // 解析UnitPath
            const unitPathRegex = /<UnitPath[^>]*>[\s\S]*?<Value>([^<]*)<\/Value>/;
            const unitPathMatch = content.match(unitPathRegex);
            if (unitPathMatch) {
                const paths = unitPathMatch[1].split(';').filter(p => p.trim());
                unitPaths.push(...paths);
            }

            // 解析LibraryPath
            const libraryPathRegex = /<LibraryPath[^>]*>[\s\S]*?<Value>([^<]*)<\/Value>/;
            const libraryPathMatch = content.match(libraryPathRegex);
            if (libraryPathMatch) {
                const paths = libraryPathMatch[1].split(';').filter(p => p.trim());
                searchPaths.push(...paths);
            }

            projectInfo.searchPaths = searchPaths;
            projectInfo.unitPaths = unitPaths;

            return projectInfo;
        } catch (error) {
            console.error('Error parsing LPI file:', error);
            return null;
        }
    }

    /**
     * 从Lazarus项目信息创建FPC任务定义
     * @param projectInfo 项目信息
     * @param lpiFilePath .lpi文件路径
     * @returns FPC任务定义
     */
    public static createTaskDefinitionFromLpi(projectInfo: LazarusProjectInfo, lpiFilePath: string): any {
        const projectDir = path.dirname(lpiFilePath);
        const mainFilePath = path.isAbsolute(projectInfo.mainFile) 
            ? projectInfo.mainFile 
            : path.join(projectDir, projectInfo.mainFile);

        const taskDef = {
            label: `${projectInfo.title} (Lazarus)`,
            file: path.relative(process.cwd(), mainFilePath),
            type: 'fpc',
            buildOption: {
                unitOutputDir: './lib',
                customOptions: projectInfo.compilerOptions || [],
                searchPath: projectInfo.unitPaths || [],
                syntaxMode: 'ObjFPC',
                outputFile: projectInfo.targetFile
            }
        };

        return taskDef;
    }
}
