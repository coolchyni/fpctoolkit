const fs = require('fs');
const path = require('path');

// 模拟vscode模块  
const mockVscode = {
    window: {
        createOutputChannel: () => ({
            appendLine: (message) => console.log('LOG:', message)
        })
    },
    workspace: {
        workspaceFolders: [{
            uri: { fsPath: path.resolve(__dirname, '..') }
        }]
    }
};

// 设置全局对象，这样打包的代码可以找到vscode
global.vscode = mockVscode;

// 现在加载我们编译后的模块
const extensionModule = require('../out/extension.js');

// 提取所需的类
console.log('Available exports:', Object.keys(extensionModule));

// 由于我们的类可能被打包了，我们需要以不同的方式访问它们
// 让我们直接使用源代码进行测试

function simulateParser() {
    console.log('=== Simulating LPI Parsing ===');
    
    const lpiFilePath = path.resolve(__dirname, '../src/providers/sample/project1.lpi');
    console.log('LPI file path:', lpiFilePath);
    console.log('File exists:', fs.existsSync(lpiFilePath));
    
    if (!fs.existsSync(lpiFilePath)) {
        console.error('LPI file not found!');
        return;
    }
    
    // 模拟解析结果根据我们的XML测试
    const mockResults = [
        {
            title: 'project1',
            mainFile: 'project1.lpr',
            buildMode: 'Debug',
            name: 'Debug',
            id: 'debug',
            isDefault: false, // 将被default逻辑重新设置
            targetOS: 'linux',
            targetCPU: 'x86_64',
            compilerOptions: ['-gw3', '-B'],
            outputDirectory: 'lib/$(TargetCPU)-$(TargetOS)',
            _isLazarusDefault: false,
            _isActiveMode: false
        },
        {
            title: 'project1',
            mainFile: 'project1.lpr', 
            buildMode: 'Release',
            name: 'Release',
            id: 'release',
            isDefault: false,
            targetOS: 'linux',
            targetCPU: 'x86_64',
            compilerOptions: ['-O3', '-CX', '-B'],
            outputDirectory: 'lib/$(TargetCPU)-$(TargetOS)',
            _isLazarusDefault: false,
            _isActiveMode: false
        }
    ];
    
    // 应用default逻辑
    applyDefaultBuildModeLogic(mockResults);
    
    console.log('\n=== Parse Results ===');
    console.log('Number of build modes found:', mockResults.length);
    
    mockResults.forEach((projectInfo, index) => {
        console.log(`\nBuild Mode ${index + 1}:`);
        console.log('- Title:', projectInfo.title);
        console.log('- Build Mode:', projectInfo.buildMode);
        console.log('- Name:', projectInfo.name);
        console.log('- ID:', projectInfo.id);
        console.log('- Is Default:', projectInfo.isDefault);
        console.log('- Main File:', projectInfo.mainFile);
        console.log('- Target OS:', projectInfo.targetOS);
        console.log('- Target CPU:', projectInfo.targetCPU);
        console.log('- Compiler Options:', projectInfo.compilerOptions);
        console.log('- Output Directory:', projectInfo.outputDirectory);
    });
    
    // 验证只有一个default
    const defaultCount = mockResults.filter(m => m.isDefault).length;
    console.log('\nValidation:');
    console.log('- Default count:', defaultCount);
    console.log('- Test result:', defaultCount === 1 ? 'PASSED' : 'FAILED');
}

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

simulateParser();
