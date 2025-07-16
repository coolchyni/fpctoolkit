/**
 * 全局持久存储，用于存储默认构建模式
 */
export class DefaultBuildModeStorage {
    private static instance: DefaultBuildModeStorage;
    private defaultBuildModeId: string = ''; // 全局默认构建模式ID
    
    private constructor() {
        // 从 VS Code 全局状态中加载数据
        this.loadFromGlobalState();
    }
    
    public static getInstance(): DefaultBuildModeStorage {
        if (!DefaultBuildModeStorage.instance) {
            DefaultBuildModeStorage.instance = new DefaultBuildModeStorage();
        }
        return DefaultBuildModeStorage.instance;
    }
    
    /**
     * 设置全局默认构建模式
     * @param buildModeId 构建模式ID
     */
    public setDefaultBuildMode(buildModeId: string): void {
        this.defaultBuildModeId = buildModeId;
        this.saveToGlobalState();
    }
    
    /**
     * 获取全局默认构建模式
     * @returns 默认构建模式ID
     */
    public getDefaultBuildMode(): string {
        return this.defaultBuildModeId;
    }
    
    /**
     * 检查构建模式是否为默认
     * @param buildModeId 构建模式ID
     * @returns 是否为默认构建模式
     */
    public isDefaultBuildMode(buildModeId: string): boolean {
        return this.defaultBuildModeId === buildModeId;
    }
    
    /**
     * 从 VS Code 全局状态中加载数据
     */
    private loadFromGlobalState(): void {
        try {
            // 获取 VS Code 扩展上下文
            const { FpcCommandManager } = require('../commands');
            const context = FpcCommandManager.context;
            
            // 从全局状态中获取数据
            const data = context.globalState.get('lazarusDefaultBuildMode');
            if (data && typeof data === 'string') {
                this.defaultBuildModeId = data;
            }
        } catch (error) {
            console.error('Error loading default build mode from global state:', error);
        }
    }
    
    /**
     * 将数据保存到 VS Code 全局状态中
     */
    private saveToGlobalState(): void {
        try {
            // 获取 VS Code 扩展上下文
            const { FpcCommandManager } = require('../commands');
            const context = FpcCommandManager.context;
            
            // 保存到全局状态
            context.globalState.update('lazarusDefaultBuildMode', this.defaultBuildModeId);
        } catch (error) {
            console.error('Error saving default build mode to global state:', error);
        }
    }
}
