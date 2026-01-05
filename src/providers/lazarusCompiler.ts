import * as vscode from 'vscode';
import * as ChildProcess from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import { TerminalEscape, TE_Style } from '../common/escape';

export interface CompilerResult {
    success: boolean;
    compiler: 'lazbuild' | 'fpc';
    command: string;
    args: string[];
}

export class LazarusCompiler {
    private static lazbuildPath: string | null = null;
    private static lazbuildChecked: boolean = false;

    /**
     * Check if lazbuild is available in the system
     */
    public static async checkLazbuildAvailability(): Promise<boolean> {
        if (this.lazbuildChecked) {
            return this.lazbuildPath !== null;
        }

        try {
            // First check if lazbuild is in PATH
            const result = ChildProcess.execSync('lazbuild --version', {
                encoding: 'utf8',
                timeout: 5000,
                stdio: ['ignore', 'pipe', 'ignore']
            });

            if (result && result.trim().length > 0) {
                this.lazbuildPath = 'lazbuild';
                this.lazbuildChecked = true;
                return true;
            }
        } catch (error) {
            // lazbuild not found in PATH or failed, try common installation paths
        }

        // Check common installation paths
        const commonPaths = this.getCommonLazbuildPaths();

        for (const lazPath of commonPaths) {
            try {
                if (fs.existsSync(lazPath)) {
                    // Just verify it's executable by checking version
                    const result = ChildProcess.execSync(`"${lazPath}" --version`, {
                        encoding: 'utf8',
                        timeout: 5000,
                        stdio: ['ignore', 'pipe', 'ignore']
                    });

                    if (result && result.trim().length > 0) {
                        this.lazbuildPath = lazPath;
                        this.lazbuildChecked = true;
                        return true;
                    }
                }
            } catch (error) {
                // Continue checking other paths even if one fails
            }
        }

        // Check LAZARUSDIR environment variable
        const lazarusDir = process.env.LAZARUSDIR || vscode.workspace.getConfiguration('fpctoolkit').get<string>('env.LAZARUSDIR');
        if (lazarusDir) {
            const lazBuildPath = path.join(lazarusDir, process.platform === 'win32' ? 'lazbuild.exe' : 'lazbuild');
            try {
                if (fs.existsSync(lazBuildPath)) {
                    const result = ChildProcess.execSync(`"${lazBuildPath}" --version`, {
                        encoding: 'utf8',
                        timeout: 5000,
                        stdio: ['ignore', 'pipe', 'ignore']
                    });

                    if (result && result.trim().length > 0) {
                        this.lazbuildPath = lazBuildPath;
                        this.lazbuildChecked = true;
                        return true;
                    }
                }
            } catch (error) {
                // Continue to fallback
            }
        }

        this.lazbuildChecked = true;
        this.lazbuildPath = null;
        return false;
    }

    /**
     * Get common lazbuild installation paths based on platform
     */
    private static getCommonLazbuildPaths(): string[] {
        const paths: string[] = [];

        switch (process.platform) {
            case 'win32':
                paths.push(
                    'C:\\lazarus\\lazbuild.exe',
                    'C:\\Program Files\\Lazarus\\lazbuild.exe',
                    'C:\\Program Files (x86)\\Lazarus\\lazbuild.exe'
                );
                break;
            case 'darwin':
                paths.push(
                    '/usr/local/bin/lazbuild',
                    '/opt/local/bin/lazbuild',
                    '/Applications/Lazarus/lazbuild',
                    '/Applications/lazarus/lazbuild'
                );
                break;
            case 'linux':
                paths.push(
                    '/usr/bin/lazbuild',
                    '/usr/local/bin/lazbuild',
                    '/opt/lazarus/lazbuild'
                );
                break;
        }

        return paths;
    }

    /**
     * Get the lazbuild path if available
     */
    public static getLazbuildPath(): string | null {
        return this.lazbuildPath;
    }

    /**
     * Build Lazarus project using lazbuild
     */
    public static async buildWithLazbuild(
        projectFile: string,
        buildMode: string,
        workspaceRoot: string,
        forceRebuild: boolean = false
    ): Promise<CompilerResult> {
        const lazbuildPath = this.getLazbuildPath();
        if (!lazbuildPath) {
            throw new Error('lazbuild not available');
        }

        const args: string[] = [];

        // Add build mode if specified and not default
        if (buildMode && buildMode !== 'Default' && buildMode.trim() !== '') {
            args.push('--build-mode=' + buildMode);
        }

        // Add force rebuild flag
        if (forceRebuild) {
            args.push('--build-all');
        }

        // Add quiet flag to reduce output noise
        args.push('--quiet');

        // Add project file (must be last)
        args.push(projectFile);

        console.log(`Using lazbuild: ${lazbuildPath} with args: ${args.join(' ')}`);

        return {
            success: true,
            compiler: 'lazbuild',
            command: lazbuildPath,
            args: args
        };
    }

    /**
     * Build Lazarus project using fpc as fallback
     */
    public static buildWithFpc(
        fpcPath: string,
        fpcArgs: string[],
        workspaceRoot: string
    ): CompilerResult {
        console.log(`Using fpc fallback: ${fpcPath} with args: ${fpcArgs.join(' ')}`);

        return {
            success: true,
            compiler: 'fpc',
            command: fpcPath,
            args: fpcArgs
        };
    }

    /**
     * Determine the best compilation strategy for a Lazarus project
     */
    public static async getBestCompilationStrategy(
        projectFile: string,
        buildMode: string,
        workspaceRoot: string,
        fpcPath: string,
        fpcArgs: string[],
        forceRebuild: boolean = false
    ): Promise<CompilerResult> {
        // Check if this is a Lazarus project file
        const isLazarusProject = projectFile.toLowerCase().endsWith('.lpi');

        if (isLazarusProject) {
            // Check user preference for lazbuild
            const preferLazbuild = vscode.workspace.getConfiguration('fpctoolkit.lazarus').get<boolean>('preferLazbuild', true);

            if (preferLazbuild) {
                // Try lazbuild 
                const hasLazbuild = await this.checkLazbuildAvailability();
                if (hasLazbuild) {
                    return this.buildWithLazbuild(projectFile, buildMode, workspaceRoot, forceRebuild);
                } else {
                    // No fallback for Lazarus projects if lazbuild is not found
                    throw new Error('lazbuild not found. Please ensure Lazarus is installed and lazbuild is in your PATH, or set the Lazarus directory in settings.');
                }
            } else {
                 // Even if not preferred, if it's a Lazarus project we should warn that FPC might fail
                 return this.buildWithFpc(fpcPath, fpcArgs, workspaceRoot);
            }
        }

        // Standard FPC build (not a Lazarus project)
        return this.buildWithFpc(fpcPath, fpcArgs, workspaceRoot);
    }
}