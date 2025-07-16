import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';
import { LazarusProject, LazarusUtils } from './lazarus';

/**
 * 类用于处理Lazarus项目文件中的变量替换
 */
export class LazarusVariableSubstitution {
    private static variables: Map<string, string> = new Map();
    private static projectDir: string = '';
    private static _buildMode: LazarusProject | null = null;
    private static recursionLimit: number = 10; // 防止无限递归的限制
    private static dependencyGraph: Map<string, Set<string>> = new Map(); // 变量依赖关系图

    /**
     * 初始化变量替换系统
     * @param buildMode 构建模式信息
     * @param projectDir 项目目录路径
     * @param projectName 项目名称
     * @param projectFile 项目文件名
     * @param targetFile 目标文件名
     * @param outputDirectory 输出目录
     */
    public static initialize(
        buildMode: LazarusProject | null, 
        projectDir: string,
        projectName: string,
        projectFile: string,
        targetFile?: string,
        outputDirectory?: string
    ): void {
        this.variables.clear();
        this.dependencyGraph.clear();
        this.projectDir = projectDir;
        this._buildMode = buildMode;

        // 项目相关变量
        this.variables.set('ProjPath', projectDir);
        this.variables.set('ProjFile', projectFile);
        this.variables.set('ProjName', projectName);
        this.variables.set('ProjDir', projectDir);
        
        // 输出目录变量
        if (outputDirectory) {
            this.variables.set('ProjOutDir', outputDirectory);
        } else {
            this.variables.set('ProjOutDir', path.join(projectDir, 'lib'));
        }

        // 获取系统默认值
        const systemDefaults = LazarusUtils.getSystemDefaults();
        
        // 目标平台变量
        const targetOS = buildMode?.targetOS || systemDefaults.targetOS;
        const targetCPU = buildMode?.targetCPU || systemDefaults.targetCPU;
        
        this.variables.set('TargetOS', targetOS);
        this.variables.set('TargetCPU', targetCPU);
        
        // 构建模式变量
        if (buildMode) {
            this.variables.set('BuildMode', buildMode.name || 'Default');
        } else {
            this.variables.set('BuildMode', 'Default');
        }
        
        // 目标文件变量
        if (targetFile) {
            this.variables.set('TargetFile', targetFile);
            
            // 提取目标文件扩展名
            const targetExt = path.extname(targetFile);
            if (targetExt) {
                this.variables.set('TargetExt', targetExt);
            }
        }
        
        // 使用增强的系统检测
        this.variables.set('LazarusDir', systemDefaults.lazarusDir || '');
        this.variables.set('FPCVer', systemDefaults.fpcVersion || '');
        
        // 应用程序目录（在VS Code扩展中通常是工作区根目录）
        this.variables.set('AppDir', projectDir);
        
        // 添加环境变量
        this.addEnvironmentVariables();
        
        // 构建变量依赖关系图
        this.buildDependencyGraph();
    }

    /**
     * 替换路径中的变量
     * @param inputPath 包含变量的路径
     * @returns 替换变量后的路径
     */
    public static substitute(inputPath: string): string {
        if (!inputPath || (!inputPath.includes('$(') && !inputPath.includes('$ENV('))) {
            return inputPath;
        }

        let result = inputPath;
        let recursionCount = 0;
        
        // 继续替换，直到没有更多变量或达到递归限制
        while ((result.includes('$(') || result.includes('$ENV(')) && recursionCount < this.recursionLimit) {
            recursionCount++;
            
            // 替换 $ENV(变量名) 格式的环境变量
            result = result.replace(/\$ENV\(([^)]+)\)/g, (match, envVarName) => {
                const envValue = process.env[envVarName];
                return envValue !== undefined ? envValue : match;
            });
            
            // 替换 $(变量名) 格式的变量
            result = result.replace(/\$\(([^)]+)\)/g, (match, varName) => {
                const value = this.getVariableValue(varName);
                return value !== undefined ? value : match;
            });
        }
        
        // 如果达到递归限制但仍有变量，记录警告
        if ((result.includes('$(') || result.includes('$ENV(')) && recursionCount >= this.recursionLimit) {
            console.warn(`变量替换达到递归限制 (${this.recursionLimit})，可能存在循环引用: ${inputPath}`);
        }
        
        // 规范化路径分隔符
        result = this.normalizePath(result);
        
        return result;
    }

    /**
     * 替换多个路径中的变量
     * @param paths 包含变量的路径数组
     * @returns 替换变量后的路径数组
     */
    public static substitutePaths(paths: string[]): string[] {
        return paths.map(p => this.substitute(p));
    }

    /**
     * 获取变量值
     * @param name 变量名
     * @returns 变量值，如果不存在则返回undefined
     */
    private static getVariableValue(name: string): string | undefined {
        // 首先检查我们的变量映射
        if (this.variables.has(name)) {
            return this.variables.get(name);
        }
        
        // 检查是否是环境变量引用
        const envValue = process.env[name];
        if (envValue !== undefined) {
            return envValue;
        }
        
        // 变量不存在，返回undefined
        return undefined;
    }

    /**
     * 添加环境变量作为可用变量
     */
    private static addEnvironmentVariables(): void {
        // 添加一些常用的环境变量
        const commonEnvVars = ['HOME', 'TEMP', 'TMP', 'USERPROFILE', 'PATH', 'APPDATA', 'LOCALAPPDATA', 'HOMEDRIVE', 'HOMEPATH'];
        
        for (const envVar of commonEnvVars) {
            const value = process.env[envVar];
            if (value) {
                this.variables.set(envVar, value);
            }
        }
    }

    /**
     * 构建变量依赖关系图
     * 用于检测变量之间的依赖关系，以便正确处理嵌套变量
     */
    private static buildDependencyGraph(): void {
        this.dependencyGraph.clear();
        
        // 遍历所有变量
        for (const [varName, varValue] of this.variables.entries()) {
            // 如果变量值包含其他变量引用
            if (varValue && varValue.includes('$(')) {
                // 创建一个依赖集合
                const dependencies = new Set<string>();
                
                // 查找所有依赖的变量
                const matches = varValue.match(/\$\(([^)]+)\)/g);
                if (matches) {
                    for (const match of matches) {
                        // 提取变量名（去掉$()）
                        const depVarName = match.substring(2, match.length - 1);
                        dependencies.add(depVarName);
                    }
                }
                
                // 存储依赖关系
                this.dependencyGraph.set(varName, dependencies);
            }
        }
        
        // 检测循环依赖
        this.detectCyclicDependencies();
    }

    /**
     * 检测变量之间的循环依赖
     */
    private static detectCyclicDependencies(): void {
        // 对每个变量执行深度优先搜索
        const visited = new Set<string>();
        const recursionStack = new Set<string>();
        
        for (const varName of this.dependencyGraph.keys()) {
            if (!visited.has(varName)) {
                this.dfsForCyclicDetection(varName, visited, recursionStack);
            }
        }
    }

    /**
     * 深度优先搜索检测循环依赖
     * @param varName 当前变量名
     * @param visited 已访问的变量集合
     * @param recursionStack 递归栈
     * @returns 是否存在循环依赖
     */
    private static dfsForCyclicDetection(
        varName: string, 
        visited: Set<string>, 
        recursionStack: Set<string>
    ): boolean {
        // 标记为已访问
        visited.add(varName);
        recursionStack.add(varName);
        
        // 获取依赖
        const dependencies = this.dependencyGraph.get(varName);
        if (dependencies) {
            for (const dep of dependencies) {
                // 如果依赖在递归栈中，则存在循环
                if (recursionStack.has(dep)) {
                    console.warn(`检测到变量循环依赖: ${varName} -> ${dep}`);
                    // 打破循环依赖
                    this.breakCyclicDependency(varName, dep);
                    return true;
                }
                
                // 如果依赖未访问，则继续DFS
                if (!visited.has(dep) && this.dfsForCyclicDetection(dep, visited, recursionStack)) {
                    return true;
                }
            }
        }
        
        // 从递归栈中移除
        recursionStack.delete(varName);
        return false;
    }

    /**
     * 打破循环依赖
     * @param var1 变量1
     * @param var2 变量2
     */
    private static breakCyclicDependency(var1: string, var2: string): void {
        // 简单的解决方案：移除var1对var2的依赖
        const dependencies = this.dependencyGraph.get(var1);
        if (dependencies) {
            dependencies.delete(var2);
            
            // 如果var1的值包含var2的引用，则替换为空字符串
            const value = this.variables.get(var1);
            if (value) {
                const newValue = value.replace(`$(${var2})`, '');
                this.variables.set(var1, newValue);
            }
        }
    }

    /**
     * 规范化路径（处理不同操作系统的路径分隔符）
     * @param inputPath 输入路径
     * @returns 规范化后的路径
     */
    private static normalizePath(inputPath: string): string {
        // 如果路径为空，直接返回
        if (!inputPath) {
            return inputPath;
        }
        
        // 使用path.normalize处理路径
        let normalizedPath = path.normalize(inputPath);
        
        // 在Windows上，确保使用正斜杠（Lazarus项目通常使用正斜杠）
        if (os.platform() === 'win32') {
            normalizedPath = normalizedPath.replace(/\\/g, '/');
        }
        
        return normalizedPath;
    }

    /**
     * 解析相对路径（相对于项目目录）
     * @param relativePath 相对路径
     * @returns 绝对路径
     */
    public static resolveRelativePath(relativePath: string): string {
        // 首先替换变量
        const substitutedPath = this.substitute(relativePath);
        
        // 如果是绝对路径，直接返回
        if (path.isAbsolute(substitutedPath)) {
            return substitutedPath;
        }
        
        // 相对于项目目录解析路径
        return path.resolve(this.projectDir, substitutedPath);
    }

    /**
     * 获取当前系统的默认目标平台信息
     * @returns 包含目标OS和CPU的对象
     */
    public static getSystemDefaults(): { targetOS: string, targetCPU: string } {
        return LazarusUtils.getSystemDefaults();
    }

    /**
     * 获取所有已定义变量的列表
     * @returns 变量名和值的映射
     */
    public static getAllVariables(): Map<string, string> {
        return new Map(this.variables);
    }

    /**
     * 设置自定义变量
     * @param name 变量名
     * @param value 变量值
     */
    public static setVariable(name: string, value: string): void {
        this.variables.set(name, value);
        
        // 更新依赖关系图
        if (value && value.includes('$(')) {
            const dependencies = new Set<string>();
            
            // 查找所有依赖的变量
            const matches = value.match(/\$\(([^)]+)\)/g);
            if (matches) {
                for (const match of matches) {
                    // 提取变量名（去掉$()）
                    const depVarName = match.substring(2, match.length - 1);
                    dependencies.add(depVarName);
                }
            }
            
            // 存储依赖关系
            this.dependencyGraph.set(name, dependencies);
            
            // 检测循环依赖
            this.detectCyclicDependencies();
        }
    }
    
    /**
     * 检查路径是否存在
     * @param inputPath 输入路径（可能包含变量）
     * @returns 路径是否存在
     */
    public static pathExists(inputPath: string): boolean {
        // 替换变量
        const resolvedPath = this.resolveRelativePath(inputPath);
        
        // 检查路径是否存在
        try {
            return fs.existsSync(resolvedPath);
        } catch (error) {
            return false;
        }
    }
    
    /**
     * 创建目录（如果不存在）
     * @param dirPath 目录路径（可能包含变量）
     * @returns 是否成功创建目录
     */
    public static ensureDirectoryExists(dirPath: string): boolean {
        // 替换变量
        const resolvedPath = this.resolveRelativePath(dirPath);
        
        // 检查目录是否存在
        if (fs.existsSync(resolvedPath)) {
            return true;
        }
        
        // 创建目录
        try {
            fs.mkdirSync(resolvedPath, { recursive: true });
            return true;
        } catch (error) {
            console.error(`创建目录失败: ${resolvedPath}`, error);
            return false;
        }
    }
}