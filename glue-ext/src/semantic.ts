declare const vscode: any;

interface DocumentSemanticTokensProvider {
    provideDocumentSemanticTokens(document: any): any;
}

interface SemanticTokensLegend {
    constructor(tokenTypes: string[], tokenModifiers: string[]): void;
}

interface SemanticTokensBuilder {
    constructor(legend: any): void;
    push(range: any, tokenType: string): void;
    build(): any;
}

interface Range {
    constructor(start: any, end: any): void;
}

interface TextDocument {
    getText(): string;
    positionAt(offset: number): any;
}

interface ProviderResult<T> {
    // any
}

export class GlueSemanticTokensProvider implements DocumentSemanticTokensProvider {
    constructor(private legend: any) { }
    provideDocumentSemanticTokens(document: any): any {
        const builder = new vscode.SemanticTokensBuilder(this.legend);
        const text = document.getText();
        const keywords = new Set(['set', 'lambda', 'error']);

        let offset = 0;
        let inList = false;
        let listPosition = 0;

        while (offset < text.length) {
            const char = text[offset];
            if (char === ';') {
                // Comment
                while (offset < text.length && text[offset] !== '\n') {
                    offset++;
                }
            } else if (char === '"') {
                // String
                const start = offset;
                offset++;
                while (offset < text.length && text[offset] !== '"') {
                    if (text[offset] === '\\') offset++;
                    offset++;
                }
                if (offset < text.length) offset++;
                builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'string');
            } else if (char === '(') {
                inList = true;
                listPosition = 1;
                offset++;
            } else if (char === ')') {
                inList = false;
                listPosition = 0;
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
            } else if (/[0-9]/.test(char)) {
                // Number
                const start = offset;
                while (offset < text.length && /[0-9]/.test(text[offset])) {
                    offset++;
                }
                builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'number');
            } else if (!/\s/.test(char) && char !== '(' && char !== ')' && char !== '"' && char !== ';' && char !== ':') {
                // Symbol
                const start = offset;
                while (offset < text.length && !/\s/.test(text[offset]) && text[offset] !== '(' && text[offset] !== ')' && text[offset] !== '"' && text[offset] !== ';') {
                    offset++;
                }
                let symbol = text.substring(start, offset);
                // Check if it's a boolean
                if (symbol === 'true' || symbol === 'false') {
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'boolean');
                } else if (keywords.has(symbol)) {
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'keyword');
                } else if (/^[!&|<>=%+\-*\/]+$/.test(symbol[0])) {
                    // Operator (starts with special char)
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'operator');
                } else if (inList && listPosition === 1) {
                    // First symbol in list is a function
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'function');
                } else {
                    // Other symbols are variables
                    builder.push(new vscode.Range(document.positionAt(start), document.positionAt(offset)), 'variable');
                }
                if (inList) {
                    listPosition++;
                }
            } else {
                offset++;
            }
        }

        return builder.build();
    }
}
