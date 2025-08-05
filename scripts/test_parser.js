const fs = require('fs');
const path = require('path');

// 现在可以安全地导入我们的模块
const { XMLParser } = require('fast-xml-parser');

// 简化的解析逻辑测试
function testLpiParsing() {
    const lpiFilePath = '../src/providers/sample/project1.lpi';
    
    if (!fs.existsSync(lpiFilePath)) {
        console.log('LPI file not found:', lpiFilePath);
        return;
    }
    
    const content = fs.readFileSync(lpiFilePath, 'utf8');
    
    const parser = new XMLParser({
        ignoreAttributes: false,
        attributeNamePrefix: '@_',
        allowBooleanAttributes: true,
        parseAttributeValue: true,
        trimValues: true
    });
    
    const xmlObj = parser.parse(content);
    
    console.log('=== XML Object Structure ===');
    console.log('CONFIG exists:', !!xmlObj.CONFIG);
    
    if (xmlObj.CONFIG?.ProjectOptions) {
        const projectOptions = xmlObj.CONFIG.ProjectOptions;
        
        console.log('Title:', projectOptions.Title?.['@_Value']);
        console.log('BuildModes exists:', !!projectOptions.BuildModes);
        
        if (projectOptions.BuildModes) {
            const modes = projectOptions.BuildModes;
            console.log('BuildModes structure:');
            console.log('- BuildMode exists:', !!modes.BuildMode);
            console.log('- Item exists:', !!modes.Item);
            
            if (modes.Item) {
                if (Array.isArray(modes.Item)) {
                    console.log('- Items count:', modes.Item.length);
                    modes.Item.forEach((item, index) => {
                        console.log(`  Item ${index + 1}:`, {
                            Name: item['@_Name'],
                            Default: item['@_Default'],
                            hasCompilerOptions: !!item.CompilerOptions
                        });
                    });
                } else {
                    console.log('- Single item:', {
                        Name: modes.Item['@_Name'],
                        Default: modes.Item['@_Default'],
                        hasCompilerOptions: !!modes.Item.CompilerOptions
                    });
                }
            }
        }
        
        // 检查 Units
        if (projectOptions.Units) {
            console.log('Units structure:');
            if (projectOptions.Units.Unit) {
                if (Array.isArray(projectOptions.Units.Unit)) {
                    console.log('- Units count:', projectOptions.Units.Unit.length);
                    projectOptions.Units.Unit.forEach((unit, index) => {
                        console.log(`  Unit ${index}:`, {
                            Filename: unit.Filename?.['@_Value'],
                            IsPartOfProject: unit.IsPartOfProject?.['@_Value']
                        });
                    });
                } else {
                    console.log('- Single unit:', {
                        Filename: projectOptions.Units.Unit.Filename?.['@_Value'],
                        IsPartOfProject: projectOptions.Units.Unit.IsPartOfProject?.['@_Value']
                    });
                }
            }
        }
    }
}

testLpiParsing();
