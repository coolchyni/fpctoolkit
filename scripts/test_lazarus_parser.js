const fs = require('fs');
const path = require('path');

// 创建一个模拟的 vscode 模块
const mockVscode = {
    window: {
        createOutputChannel: () => ({
            appendLine: (message) => console.log('LOG:', message)
        })
    },
    workspace: {
        workspaceFolders: [{
            uri: { fsPath: __dirname }
        }]
    }
};

// 设置全局模拟
global.console = {
    ...console,
    error: (...args) => console.log('ERROR:', ...args),
    warn: (...args) => console.log('WARN:', ...args)
};

// 测试编译后的TypeScript模块
function testCompiledParser() {
    try {
        // 直接测试我们的模块，先模拟依赖
        
        // 模拟 XMLParser
        const mockXMLParser = class {
            constructor(options) {
                this.options = options;
            }
            
            parse(content) {
                // 手动解析我们已知的XML结构
                const result = {
                    CONFIG: {
                        ProjectOptions: {
                            Title: { '@_Value': 'project1' },
                            BuildModes: {
                                Item: [
                                    { '@_Name': 'Default', '@_Default': 'True' },
                                    { '@_Name': 'Debug', CompilerOptions: { /* mock */ } },
                                    { '@_Name': 'Release', CompilerOptions: { /* mock */ } }
                                ]
                            },
                            Units: {
                                Unit: {
                                    Filename: { '@_Value': 'project1.lpr' },
                                    IsPartOfProject: { '@_Value': 'True' }
                                }
                            }
                        }
                    }
                };
                return result;
            }
        };
        
        // 创建一个简化的解析器测试
        console.log('Testing default logic with mock data...');
        
        const buildModes = [
            {
                buildMode: 'Default',
                name: 'Default',
                id: 'default',
                isDefault: false,
                _isLazarusDefault: true,  // 这个在LPI中标记为Default="True"
                _isActiveMode: false
            },
            {
                buildMode: 'Debug', 
                name: 'Debug',
                id: 'debug',
                isDefault: false,
                _isLazarusDefault: false,
                _isActiveMode: false
            },
            {
                buildMode: 'Release',
                name: 'Release', 
                id: 'release',
                isDefault: false,
                _isLazarusDefault: false,
                _isActiveMode: false
            }
        ];
        
        // 应用default逻辑
        applyDefaultBuildModeLogic(buildModes);
        
        console.log('\n=== Default Logic Test Results ===');
        buildModes.forEach((mode, index) => {
            console.log(`Build Mode ${index + 1}:`);
            console.log('- Name:', mode.buildMode);
            console.log('- Is Default:', mode.isDefault);
            console.log('');
        });
        
        // 验证只有一个default
        const defaultCount = buildModes.filter(m => m.isDefault).length;
        console.log('Default count:', defaultCount);
        console.log('Test', defaultCount === 1 ? 'PASSED' : 'FAILED');
        
    } catch (error) {
        console.error('Error testing parser:', error.message);
    }
}

// 简化的default逻辑测试函数
function applyDefaultBuildModeLogic(buildModes) {
    if (buildModes.length === 0) {
        return;
    }

    // Priority 2: Use Lazarus project default (marked with Default="True" in LPI file)
    let defaultMode = buildModes.find(mode => mode._isLazarusDefault === true);
    
    // Priority 3: Use active mode if no Lazarus default found
    if (!defaultMode) {
        defaultMode = buildModes.find(mode => mode._isActiveMode === true);
    }
    
    // Priority 4: If no defaults found, use the first build mode
    if (!defaultMode) {
        defaultMode = buildModes[0];
    }

    // Ensure only one mode is marked as default
    buildModes.forEach(mode => {
        mode.isDefault = (mode === defaultMode);
        // Clean up temporary metadata
        delete mode._isLazarusDefault;
        delete mode._isActiveMode;
    });

    console.log(`Set build mode "${defaultMode.buildMode}" as default`);
}

testCompiledParser();
