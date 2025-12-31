import * as path from 'path';
import * as os from 'os';
import * as fs from 'fs';
import { LazarusProject, LazarusUtils } from './lazarus';

/**
 * Class for handling variable substitution in Lazarus project files
 */
export class LazarusVariableSubstitution {
    private static variables: Map<string, string> = new Map();
    private static projectDir: string = '';
    private static _buildMode: { targetOS?: string, targetCPU?: string, name?: string } | null = null;
    private static recursionLimit: number = 10; // Limit to prevent infinite recursion
    private static dependencyGraph: Map<string, Set<string>> = new Map(); // Variable dependency graph

    /**
     * Initialize variable substitution system
     * @param buildMode Build mode information
     * @param projectDir Project directory path
     * @param projectName Project name
     * @param projectFile Project file name
     * @param targetFile Target file name
     * @param outputDirectory Output directory
     */
    public static initialize(
        buildMode: { targetOS?: string, targetCPU?: string, name?: string } | null, 
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

        // Project-related variables
        this.variables.set('ProjPath', projectDir);
        this.variables.set('ProjFile', projectFile);
        this.variables.set('ProjName', projectName);
        this.variables.set('ProjDir', projectDir);
        
        // Output directory variables
        if (outputDirectory) {
            this.variables.set('ProjOutDir', outputDirectory);
        } else {
            this.variables.set('ProjOutDir', path.join(projectDir, 'lib'));
        }

        // Get system defaults
        const systemDefaults = LazarusUtils.getSystemDefaults();
        
        // Target platform variables
        const targetOS = buildMode?.targetOS || systemDefaults.targetOS;
        const targetCPU = buildMode?.targetCPU || systemDefaults.targetCPU;
        
        this.variables.set('TargetOS', targetOS);
        this.variables.set('TargetCPU', targetCPU);
        
        // Build mode variables
        if (buildMode) {
            this.variables.set('BuildMode', buildMode.name || 'Default');
        } else {
            this.variables.set('BuildMode', 'Default');
        }
        
        // Target file variables
        if (targetFile) {
            this.variables.set('TargetFile', targetFile);
            
            // Extract target file extension
            const targetExt = path.extname(targetFile);
            if (targetExt) {
                this.variables.set('TargetExt', targetExt);
            }
        }
        
        // Use enhanced system detection
        this.variables.set('LazarusDir', systemDefaults.lazarusDir || '');
        this.variables.set('FPCVer', systemDefaults.fpcVersion || '');
        
        // Application directory (usually workspace root in VS Code extension)
        this.variables.set('AppDir', projectDir);
        
        // Add environment variables
        this.addEnvironmentVariables();
        
        // Build variable dependency graph
        this.buildDependencyGraph();
    }

    /**
     * Replace variables in path
     * @param inputPath Path containing variables
     * @returns Path with variables replaced
     */
    public static substitute(inputPath: string): string {
        if (!inputPath || (!inputPath.includes('$(') && !inputPath.includes('$ENV('))) {
            return inputPath;
        }

        let result = inputPath;
        let recursionCount = 0;
        
        // Continue replacing until no more variables or recursion limit reached
        while ((result.includes('$(') || result.includes('$ENV(')) && recursionCount < this.recursionLimit) {
            recursionCount++;
            
            // Replace environment variables in $ENV(variable_name) format
            result = result.replace(/\$ENV\(([^)]+)\)/g, (match, envVarName) => {
                const envValue = process.env[envVarName];
                return envValue !== undefined ? envValue : match;
            });
            
            // Replace variables in $(variable_name) format
            result = result.replace(/\$\(([^)]+)\)/g, (match, varName) => {
                const value = this.getVariableValue(varName);
                return value !== undefined ? value : match;
            });
        }
        
        // If recursion limit reached but variables still exist, log warning
        if ((result.includes('$(') || result.includes('$ENV(')) && recursionCount >= this.recursionLimit) {
            console.warn(`Variable substitution reached recursion limit (${this.recursionLimit}), possible circular reference: ${inputPath}`);
        }
        
        // Normalize path separators
        result = this.normalizePath(result);
        
        return result;
    }

    /**
     * Replace variables in multiple paths
     * @param paths Array of paths containing variables
     * @returns Array of paths with variables replaced
     */
    public static substitutePaths(paths: string[]): string[] {
        return paths.map(p => this.substitute(p));
    }

    /**
     * Get variable value
     * @param name Variable name
     * @returns Variable value, or undefined if not exists
     */
    private static getVariableValue(name: string): string | undefined {
        // First check our variable mapping
        if (this.variables.has(name)) {
            return this.variables.get(name);
        }
        
        // Check if it's an environment variable reference
        const envValue = process.env[name];
        if (envValue !== undefined) {
            return envValue;
        }
        
        // Variable doesn't exist, return undefined
        return undefined;
    }

    /**
     * Add environment variables as available variables
     */
    private static addEnvironmentVariables(): void {
        // Add some common environment variables
        const commonEnvVars = ['HOME', 'TEMP', 'TMP', 'USERPROFILE', 'PATH', 'APPDATA', 'LOCALAPPDATA', 'HOMEDRIVE', 'HOMEPATH'];
        
        for (const envVar of commonEnvVars) {
            const value = process.env[envVar];
            if (value) {
                this.variables.set(envVar, value);
            }
        }
    }

    /**
     * Build variable dependency graph
     * Used to detect dependencies between variables for proper handling of nested variables
     */
    private static buildDependencyGraph(): void {
        this.dependencyGraph.clear();
        
        // Iterate through all variables
        for (const [varName, varValue] of this.variables.entries()) {
            // If variable value contains other variable references
            if (varValue && varValue.includes('$(')) {
                // Create a dependency set
                const dependencies = new Set<string>();
                
                // Find all dependent variables
                const matches = varValue.match(/\$\(([^)]+)\)/g);
                if (matches) {
                    for (const match of matches) {
                        // Extract variable name (remove $())
                        const depVarName = match.substring(2, match.length - 1);
                        dependencies.add(depVarName);
                    }
                }
                
                // Store dependency relationship
                this.dependencyGraph.set(varName, dependencies);
            }
        }
        
        // Detect circular dependencies
        this.detectCyclicDependencies();
    }

    /**
     * Detect circular dependencies between variables
     */
    private static detectCyclicDependencies(): void {
        // Perform depth-first search for each variable
        const visited = new Set<string>();
        const recursionStack = new Set<string>();
        
        for (const varName of this.dependencyGraph.keys()) {
            if (!visited.has(varName)) {
                this.dfsForCyclicDetection(varName, visited, recursionStack);
            }
        }
    }

    /**
     * Depth-first search to detect circular dependencies
     * @param varName Current variable name
     * @param visited Set of visited variables
     * @param recursionStack Recursion stack
     * @returns Whether circular dependency exists
     */
    private static dfsForCyclicDetection(
        varName: string, 
        visited: Set<string>, 
        recursionStack: Set<string>
    ): boolean {
        // Mark as visited
        visited.add(varName);
        recursionStack.add(varName);
        
        // Get dependencies
        const dependencies = this.dependencyGraph.get(varName);
        if (dependencies) {
            for (const dep of dependencies) {
                // If dependency is in recursion stack, circular dependency exists
                if (recursionStack.has(dep)) {
                    console.warn(`Detected variable circular dependency: ${varName} -> ${dep}`);
                    // Break circular dependency
                    this.breakCyclicDependency(varName, dep);
                    return true;
                }
                
                // If dependency is unvisited, continue DFS
                if (!visited.has(dep) && this.dfsForCyclicDetection(dep, visited, recursionStack)) {
                    return true;
                }
            }
        }
        
        // Remove from recursion stack
        recursionStack.delete(varName);
        return false;
    }

    /**
     * Break circular dependency
     * @param var1 Variable 1
     * @param var2 Variable 2
     */
    private static breakCyclicDependency(var1: string, var2: string): void {
        // Simple solution: remove var1's dependency on var2
        const dependencies = this.dependencyGraph.get(var1);
        if (dependencies) {
            dependencies.delete(var2);
            
            // If var1's value contains var2 reference, replace with empty string
            const value = this.variables.get(var1);
            if (value) {
                const newValue = value.replace(`$(${var2})`, '');
                this.variables.set(var1, newValue);
            }
        }
    }

    /**
     * Normalize path (handle path separators for different operating systems)
     * @param inputPath Input path
     * @returns Normalized path
     */
    private static normalizePath(inputPath: string): string {
        // If path is empty, return directly
        if (!inputPath) {
            return inputPath;
        }
        
        // Use path.normalize to handle path
        let normalizedPath = path.normalize(inputPath);
        
        // On Windows, ensure forward slashes are used (Lazarus projects typically use forward slashes)
        if (os.platform() === 'win32') {
            normalizedPath = normalizedPath.replace(/\\/g, '/');
        }
        
        return normalizedPath;
    }

    /**
     * Resolve relative path (relative to project directory)
     * @param relativePath Relative path
     * @returns Absolute path
     */
    public static resolveRelativePath(relativePath: string): string {
        // First replace variables
        const substitutedPath = this.substitute(relativePath);
        
        // If it's an absolute path, return directly
        if (path.isAbsolute(substitutedPath)) {
            return substitutedPath;
        }
        
        // Resolve path relative to project directory
        return path.resolve(this.projectDir, substitutedPath);
    }

    /**
     * Get default target platform information for current system
     * @returns Object containing target OS and CPU
     */
    public static getSystemDefaults(): { targetOS: string, targetCPU: string } {
        return LazarusUtils.getSystemDefaults();
    }

    /**
     * Get list of all defined variables
     * @returns Mapping of variable names and values
     */
    public static getAllVariables(): Map<string, string> {
        return new Map(this.variables);
    }

    /**
     * Set custom variable
     * @param name Variable name
     * @param value Variable value
     */
    public static setVariable(name: string, value: string): void {
        this.variables.set(name, value);
        
        // Update dependency graph
        if (value && value.includes('$(')) {
            const dependencies = new Set<string>();
            
            // Find all dependent variables
            const matches = value.match(/\$\(([^)]+)\)/g);
            if (matches) {
                for (const match of matches) {
                    // Extract variable name (remove $())
                    const depVarName = match.substring(2, match.length - 1);
                    dependencies.add(depVarName);
                }
            }
            
            // Store dependency relationship
            this.dependencyGraph.set(name, dependencies);
            
            // Detect circular dependencies
            this.detectCyclicDependencies();
        }
    }
    
    /**
     * Check if path exists
     * @param inputPath Input path (may contain variables)
     * @returns Whether path exists
     */
    public static pathExists(inputPath: string): boolean {
        // Replace variables
        const resolvedPath = this.resolveRelativePath(inputPath);
        
        // Check if path exists
        try {
            return fs.existsSync(resolvedPath);
        } catch (error) {
            return false;
        }
    }
    
    /**
     * Create directory (if it doesn't exist)
     * @param dirPath Directory path (may contain variables)
     * @returns Whether directory was successfully created
     */
    public static ensureDirectoryExists(dirPath: string): boolean {
        // Replace variables
        const resolvedPath = this.resolveRelativePath(dirPath);
        
        // Check if directory exists
        if (fs.existsSync(resolvedPath)) {
            return true;
        }
        
        // Create directory
        try {
            fs.mkdirSync(resolvedPath, { recursive: true });
            return true;
        } catch (error) {
            console.error(`Failed to create directory: ${resolvedPath}`, error);
            return false;
        }
    }
}