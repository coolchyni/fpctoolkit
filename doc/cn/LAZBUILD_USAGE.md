# Lazbuild 使用指南

本扩展支持使用 `lazbuild` 和 `fpc` 编译器编译 Lazarus 项目，并提供自动回退功能。

## 概述

当构建 Lazarus 项目（`.lpi` 或 `.lpr` 文件）时，扩展按以下优先级执行：

1. **首选**: 如果可用且首选，使用 `lazbuild`
2. **回退**: 使用 `fpc` 编译器和提取的构建选项

## 配置

### 首选 lazbuild

您可以通过以下配置控制是否优先使用 `lazbuild` 而不是 `fpc`：

```json
{
    "fpctoolkit.lazarus.preferLazbuild": true
}
```

- `true`（默认）：首先尝试 `lazbuild`，回退到 `fpc`
- `false`：始终使用 `fpc` 编译器

### Lazbuild 检测

扩展按以下顺序自动检测 `lazbuild`：

1. 检查系统 PATH 中是否有 `lazbuild`
2. 检查常见安装路径：
   - **Windows**: `C:\lazarus\lazbuild.exe`, `C:\Program Files\Lazarus\lazbuild.exe`
   - **macOS**: `/usr/local/bin/lazbuild`, `/Applications/Lazarus/lazbuild`
   - **Linux**: `/usr/bin/lazbuild`, `/usr/local/bin/lazbuild`
3. 检查 `LAZARUSDIR` 环境变量或 `fpctoolkit.env.LAZARUSDIR` 设置

## 构建模式

使用 `lazbuild` 时，扩展会：

- 使用 `--build-mode=<mode>` 传递正确的构建模式
- 对重建操作使用 `--build-all`
- 添加 `--quiet` 标志以减少输出噪音

使用 `fpc` 回退时，扩展会：

- 从 Lazarus 项目文件中提取构建选项
- 将 Lazarus 特定设置转换为 FPC 命令行选项
- 保持与现有 FPC 工作流的兼容性

## lazbuild 的优势

使用 `lazbuild` 提供以下优势：

1. **原生 Lazarus 支持**: 自动处理 Lazarus 特定的构建配置
2. **构建模式支持**: 正确处理 `.lpi` 文件中定义的不同构建模式
3. **依赖管理**: 自动处理包依赖关系
4. **优化编译**: 使用 Lazarus 的优化构建过程

## 使用示例

### 使用 lazbuild 构建
当 `lazbuild` 可用且首选时：
```
Using lazbuild compiler
lazbuild --build-mode=Release --quiet MyProject.lpi
```

### 回退到 fpc
当 `lazbuild` 不可用或不首选时：
```
Using fpc compiler
fpc MyProject.lpr -MObjFPC -Scghi -O1 -g -gl -l -vewnhibq -Fu./lib -FU./out
```

## 故障排除

### 找不到 lazbuild

如果未检测到 `lazbuild`：

1. 确保 Lazarus 已正确安装
2. 将 Lazarus 安装目录添加到系统 PATH
3. 设置 `LAZARUSDIR` 环境变量
4. 在 VS Code 设置中配置 `fpctoolkit.env.LAZARUSDIR`

### 编译错误

扩展为两种编译器提供详细的错误报告：

- **lazbuild 错误**: 从 lazbuild 输出解析并在问题面板中显示
- **fpc 错误**: 标准 FPC 错误解析，包含行/列信息

### 回退行为

如果 `lazbuild` 失败或不可用，扩展会自动回退到 `fpc`，包括：

- 从 Lazarus 项目中提取的编译器选项
- 转换的构建设置
- 标准 FPC 错误报告

## 环境变量

扩展遵循以下环境变量：

- `LAZARUSDIR`: Lazarus 安装目录路径
- `PP`: FPC 编译器路径（回退）
- `FPCDIR`: FPC 源代码目录路径

## 命令行选项

### 使用的 lazbuild 选项

- `--build-mode=<mode>`: 指定构建模式
- `--build-all`: 强制重建所有单元
- `--quiet`: 减少输出详细程度

### FPC 回退选项

扩展自动将 Lazarus 项目设置转换为适当的 FPC 命令行选项：

- `-M<mode>`: 语法模式
- `-T<target>`: 目标操作系统
- `-P<cpu>`: 目标 CPU
- `-Fu<path>`: 单元搜索路径
- `-Fi<path>`: 包含搜索路径
- `-Fl<path>`: 库搜索路径
- `-FU<path>`: 单元输出目录
- `-o<file>`: 输出文件名