/**
 * Global persistent storage for storing the default build mode
 */
export class DefaultBuildModeStorage {
    private static instance: DefaultBuildModeStorage;
    private defaultBuildModeId: string = ''; // Global default build mode ID
    
    private constructor() {
        // Load data from VS Code global state
        this.loadFromGlobalState();
    }
    
    public static getInstance(): DefaultBuildModeStorage {
        if (!DefaultBuildModeStorage.instance) {
            DefaultBuildModeStorage.instance = new DefaultBuildModeStorage();
        }
        return DefaultBuildModeStorage.instance;
    }
    
    /**
     * Set the global default build mode
     * @param buildModeId Build mode ID
     */
    public setDefaultBuildMode(buildModeId: string): void {
        this.defaultBuildModeId = buildModeId;
        this.saveToGlobalState();
    }

    /**
     * Get the global default build mode
     * @returns Default build mode ID
     */
    public getDefaultBuildMode(): string {
        return this.defaultBuildModeId;
    }

    /**
     * Check if the build mode is the default
     * @param buildModeId Build mode ID
     * @returns Whether it is the default build mode
     */
    public isDefaultBuildMode(buildModeId: string): boolean {
        return this.defaultBuildModeId === buildModeId;
    }

    /**
     * Load data from VS Code global state
     */
    private loadFromGlobalState(): void {
        try {
            // Get VS Code extension context
            const { FpcCommandManager } = require('../commands');
            const context = FpcCommandManager.context;

            // Get data from global state
            const data = context.globalState.get('lazarusDefaultBuildMode');
            if (data && typeof data === 'string') {
                this.defaultBuildModeId = data;
            }
        } catch (error) {
            console.error('Error loading default build mode from global state:', error);
        }
    }

    /**
     * Save data to VS Code global state
     */
    private saveToGlobalState(): void {
        try {
            // Get VS Code extension context
            const { FpcCommandManager } = require('../commands');
            const context = FpcCommandManager.context;

            // Save to global state
            context.globalState.update('lazarusDefaultBuildMode', this.defaultBuildModeId);
        } catch (error) {
            console.error('Error saving default build mode to global state:', error);
        }
    }
}
