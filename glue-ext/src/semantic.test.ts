import { GlueSemanticTokensProvider } from './semantic';

// Mock vscode.TextDocument
class MockTextDocument {
    private content: string;

    constructor(content: string) {
        this.content = content;
    }

    getText(): string {
        return this.content;
    }

    positionAt(offset: number): { line: number; character: number } {
        const lines = this.content.substring(0, offset).split('\n');
        return { line: lines.length - 1, character: lines[lines.length - 1].length };
    }
}

// Mock vscode.Range
class MockRange {
    constructor(public start: { line: number; character: number }, public end: { line: number; character: number }) { }
}

// Mock vscode.SemanticTokensBuilder
class MockSemanticTokensBuilder {
    constructor(legend?: any) { } // Accept legend parameter
    private tokens: any[] = [];

    push(range: any, tokenType: string): void {
        this.tokens.push({ range, tokenType });
    }

    build(): any {
        return { tokens: this.tokens };
    }
}

// Mock vscode.SemanticTokensLegend
class MockSemanticTokensLegend {
    constructor(public tokenTypes: string[], public tokenModifiers: string[]) { }
}

// Override vscode imports for testing
const mockVscode = {
    Range: MockRange,
    SemanticTokensBuilder: MockSemanticTokensBuilder,
    SemanticTokensLegend: MockSemanticTokensLegend
};

// Monkey patch for testing
(globalThis as any).vscode = mockVscode;

// Test cases
function testSemanticProvider() {
    console.log('Testing GlueSemanticTokensProvider...');

    const legend = new MockSemanticTokensLegend(['function', 'variable', 'keyword', 'property', 'string', 'number', 'boolean', 'operator'], []);
    const provider = new GlueSemanticTokensProvider(legend as any);

    // Test 1: Function calls
    const input1 = '(def x 42)';
    const doc1 = new MockTextDocument(input1);
    const tokens1 = provider.provideDocumentSemanticTokens(doc1 as any) as any;
    console.log('Test 1 (function calls):', tokens1.tokens.length === 3 && tokens1.tokens[0].tokenType === 'function' && tokens1.tokens[1].tokenType === 'variable' && tokens1.tokens[2].tokenType === 'number' ? 'PASS' : 'FAIL');
    if (tokens1.tokens.length !== 3 || tokens1.tokens[0].tokenType !== 'function' || tokens1.tokens[1].tokenType !== 'variable' || tokens1.tokens[2].tokenType !== 'number') {
        console.log('Tokens:', tokens1.tokens);
    }

    // Test 2: Operator
    const input2 = '(+ 1 2)';
    const doc2 = new MockTextDocument(input2);
    const tokens2 = provider.provideDocumentSemanticTokens(doc2 as any) as any;
    console.log('Test 2 (operator):', tokens2.tokens.length === 3 && tokens2.tokens[0].tokenType === 'operator' && tokens2.tokens[1].tokenType === 'number' && tokens2.tokens[2].tokenType === 'number' ? 'PASS' : 'FAIL');
    if (tokens2.tokens.length !== 3 || tokens2.tokens[0].tokenType !== 'operator' || tokens2.tokens[1].tokenType !== 'number' || tokens2.tokens[2].tokenType !== 'number') {
        console.log('Tokens:', tokens2.tokens);
    }

    // Test 3: Object keys and strings
    const input3 = '(:name "Alice")';
    const doc3 = new MockTextDocument(input3);
    const tokens3 = provider.provideDocumentSemanticTokens(doc3 as any) as any;
    console.log('Test 3 (object key and string):', tokens3.tokens.length === 2 && tokens3.tokens[0].tokenType === 'property' && tokens3.tokens[1].tokenType === 'string' ? 'PASS' : 'FAIL');
    if (tokens3.tokens.length !== 2 || tokens3.tokens[0].tokenType !== 'property' || tokens3.tokens[1].tokenType !== 'string') {
        console.log('Tokens:', tokens3.tokens);
    }

    // Test 4: Mixed
    const input4 = '(def add (lambda (a b) (+ a b)))';
    const doc4 = new MockTextDocument(input4);
    const tokens4 = provider.provideDocumentSemanticTokens(doc4 as any) as any;
    // def: function, add: variable, lambda: keyword, a: function, b: variable, +: operator, a: variable, b: variable
    const expectedTypes = ['function', 'variable', 'keyword', 'function', 'variable', 'operator', 'variable', 'variable'];
    const actualTypes = tokens4.tokens.map((t: any) => t.tokenType);
    console.log('Test 4 (mixed):', JSON.stringify(actualTypes) === JSON.stringify(expectedTypes) ? 'PASS' : 'FAIL');
    if (JSON.stringify(actualTypes) !== JSON.stringify(expectedTypes)) {
        console.log('Expected:', expectedTypes);
        console.log('Got:', actualTypes);
    }
}

// Run tests
testSemanticProvider();
