import * as vscode from 'vscode';
import path = require('path');
import { configuration } from './common/configuration';
import { env } from 'process';
import * as fs from 'fs';
import { client, logger } from './extension';
import * as util from './common/util';
import {
    ExecuteCommandRequest,
    ExecuteCommandParams
} from 'vscode-languageclient/node';

export class JediFormatter {
    private default_cfg: string;
    private is_win: boolean = true;

    constructor() {
        const plat: NodeJS.Platform = process.platform;
        this.is_win = plat === 'win32';

        // 设置默认配置文件路径
        this.default_cfg = path.resolve(util.getExtensionFilePath("bin"), 'jcfsettings.cfg');
        let cfg_path = '';
        if (this.is_win) {
            cfg_path = env.LOCALAPPDATA + '/lazarus/jcfsettings.cfg';
        } else {
            cfg_path = env.HOME + '/.lazarus/jcfsettings.cfg';
        }
        if (fs.existsSync(cfg_path)) {
            this.default_cfg = cfg_path;
        }
    }

    getCfgConfig(): string {
        let cfg = configuration.get<string>("format.cfgpath", "");
        if (cfg == "") {
            cfg = this.default_cfg;
        }
        return cfg;
    }

    doInit() {
        let _this = this;
        let enable = configuration.get<boolean>('format.enabled', true);
        if (!enable) return;

        // 使用 pasls.formatCode 命令进行格式化
        vscode.languages.registerDocumentFormattingEditProvider('objectpascal', {
            async provideDocumentFormattingEdits(document: vscode.TextDocument): Promise<vscode.TextEdit[]> {
                try {
                    // 检查 LSP 客户端是否可用
                    if (!client || !client['client']) {
                        logger.appendLine('Language server client is not available for formatting');
                        return [];
                    }

                    const fileUri = document.uri.toString();
                    const cfgPath = _this.getCfgConfig();
                    const cfgUri =  vscode.Uri.file(cfgPath).toString();

                    // 调用 pasls.formatCode 命令
                    const req: ExecuteCommandParams = {
                        command: "pasls.formatCode",
                        arguments: [fileUri, cfgUri]
                    };

                    logger.appendLine(`Formatting with pasls.formatCode: ${fileUri}`);
                    await client['client']?.sendRequest(ExecuteCommandRequest.type, req);

                    return [];
                } catch (error) {
                    logger.appendLine(`Format error: ${error}`);
                    return [];
                }
            }
        });
    }
}

