import * as vscode from 'vscode';

export class GlueSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
    constructor(private legend: vscode.SemanticTokensLegend) { }
    provideDocumentSemanticTokens(document: vscode.TextDocument): vscode.ProviderResult<vscode.SemanticTokens> {
        const builder = new vscode.SemanticTokensBuilder(this.legend);
        const text = document.getText();
        const keywords = new Set(['def', 'set', 'import', 'lambda', 'if', 'module', 'export', 'try', 'catch', 'error']);

        let offset = 0;
        let inList = false;
        let firstInList = false;

        while (offset < text.length) {
            const char = text[offset];
            if (char === ';') {
                // Comment
                while (offset < text.length && text[offset] !== '\n') {
                    offset++;
                }
            } else if (char === '"') {
                // String
                offset++;
                while (offset < text.length && text[offset] !== '"') {
                    if (text[offset] === '\\') offset++;
                    offset++;
                }
                if (offset < text.length) offset++;
            } else if (char === '(') {
                inList = true;
                firstInList = true;
                offset++;
            } else if (char === ')') {
                inList = false;
                offset++;
            } else if (/\s/.test(char)) {
                offset++;
            } else if (char === ':') {
                // Object key
                const start = offset;
                offset++;
                while (offset < text.length && /[a-zA-Z0-9_\-\/.]/.test(text[offset])) {
                    offset++;
                }
                builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'property');
            } else if (/[a-zA-Z_]/.test(char)) {
                // Symbol
                const start = offset;
                while (offset < text.length && /[a-zA-Z0-9_\-]/.test(text[offset])) {
                    offset++;
                }
                let symbol = text.substring(start, offset);
                // Check if it's a keyword
                if (keywords.has(symbol)) {
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'keyword');
                } else if (inList && firstInList) {
                    // First symbol in list is a function
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'function');
                    firstInList = false;
                } else {
                    // Other symbols are variables
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'variable');
                }
            } else {
                offset++;
            }
        }

        return builder.build();
    }
}
