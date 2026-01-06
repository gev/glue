// Mock vscode for testing
const mockVscode = {
    Range: class MockRange {
        constructor(public start: { line: number; character: number }, public end: { line: number; character: number }) { }
    },
    TextEdit: {
        replace: (range: any, newText: string) => ({ range, newText })
    }
};

// Set global vscode for testing
(globalThis as any).vscode = mockVscode;

// Mock vscode parameter for formatter
const mockVscodeParam = mockVscode;

// Mock vscode.TextDocument
class MockTextDocument {
    private lines: string[];

    constructor(content: string) {
        this.lines = content.split('\n');
    }

    get lineCount(): number {
        return this.lines.length;
    }

    lineAt(index: number): { text: string; range: { start: { line: number; character: number }; end: { line: number; character: number } } } {
        const text = this.lines[index];
        return {
            text,
            range: {
                start: { line: index, character: 0 },
                end: { line: index, character: text.length }
            }
        };
    }
}

// Now import the formatter
import { formatGlueDocument } from './formatter';

// Test cases
function testFormatter() {
    console.log('Testing formatGlueDocument...');

    // Test 1: Simple nested structure
    const input1 = `(module test
(def a 1)
(def b 2)
)`;
    const expected1 = `(module test
    (def a 1)
    (def b 2)
)`;
    const doc1 = new MockTextDocument(input1);
    const edits1 = formatGlueDocument(doc1 as any, mockVscodeParam);
    const result1 = applyEdits(input1, edits1);
    console.log('Test 1:', result1 === expected1 ? 'PASS' : 'FAIL');
    if (result1 !== expected1) {
        console.log('Expected:', expected1);
        console.log('Got:', result1);
    }

    // Test 2: Already formatted
    const input2 = `(module test
    (def a 1)
    (def b 2)
)`;
    const doc2 = new MockTextDocument(input2);
    const edits2 = formatGlueDocument(doc2 as any, mockVscodeParam);
    console.log('Test 2 (already formatted):', edits2.length === 0 ? 'PASS' : 'FAIL');

    // Test 3: Complex nesting
    const input3 = `(module math
(import something)
(def add
(fn (a b)
(+ a b)
)
)
)`;
    const expected3 = `(module math
    (import something)
    (def add
        (fn (a b)
            (+ a b)
        )
    )
)`;
    const doc3 = new MockTextDocument(input3);
    const edits3 = formatGlueDocument(doc3 as any, mockVscodeParam);
    const result3 = applyEdits(input3, edits3);
    console.log('Test 3:', result3 === expected3 ? 'PASS' : 'FAIL');
    if (result3 !== expected3) {
        console.log('Expected:', expected3);
        console.log('Got:', result3);
    }

    // Test 4: Remove spaces after ( and before )
    const input4 = `( module test
( import something )
( def a 1 )
( def b ( + 1 2 ) )
)`;
    const expected4 = `(module test
    (import something)
    (def a 1)
    (def b (+ 1 2))
)`;
    const doc4 = new MockTextDocument(input4);
    const edits4 = formatGlueDocument(doc4 as any, mockVscodeParam);
    const result4 = applyEdits(input4, edits4);
    console.log('Test 4:', result4 === expected4 ? 'PASS' : 'FAIL');
    if (result4 !== expected4) {
        console.log('Expected:', expected4);
        console.log('Got:', result4);
    }

    // Test 5: User's case with nested def
    const input5 = `(module math.arithmetic
    (import ffi.math.arithmetic)

    ;; Arithmetic operations
    (def add ffi.math.arithmetic.add)
    (def + add)
    (def sub ffi.math.arithmetic.sub)
    (def - sub)
    (def mul ffi.math.arithmetic.mul)
    (def * mul)
    (def div ffi.math.arithmetic.div)
    (def /
    (div d))
(def mod ffi.math.arithmetic.mod) (def % mod))`;
    const expected5 = `(module math.arithmetic
    (import ffi.math.arithmetic)

    ;; Arithmetic operations
    (def add ffi.math.arithmetic.add)
    (def + add)
    (def sub ffi.math.arithmetic.sub)
    (def - sub)
    (def mul ffi.math.arithmetic.mul)
    (def * mul)
    (def div ffi.math.arithmetic.div)
    (def /
        (div d))
    (def mod ffi.math.arithmetic.mod) (def % mod))`;
    const doc5 = new MockTextDocument(input5);
    const edits5 = formatGlueDocument(doc5 as any, mockVscodeParam);
    const result5 = applyEdits(input5, edits5);
    console.log('Test 5:', result5 === expected5 ? 'PASS' : 'FAIL');
    if (result5 !== expected5) {
        console.log('Expected:', expected5);
        console.log('Got:', result5);
    }

    // Test 6: Line with multiple closes
    const input6 = `(module test
(def a
(lambda (x)
(if x
1
0)
)
)
)`;
    const expected6 = `(module test
    (def a
        (lambda (x)
            (if x
                1
                0)
        )
    )
)`;
    const doc6 = new MockTextDocument(input6);
    const edits6 = formatGlueDocument(doc6 as any, mockVscodeParam);
    const result6 = applyEdits(input6, edits6);
    console.log('Test 6:', result6 === expected6 ? 'PASS' : 'FAIL');
    if (result6 !== expected6) {
        console.log('Expected:', expected6);
        console.log('Got:', result6);
    }

    // Test 7: Line starting with )
    const input7 = `(module test
(def a 1)
)`;
    const expected7 = `(module test
    (def a 1)
)`;
    const doc7 = new MockTextDocument(input7);
    const edits7 = formatGlueDocument(doc7 as any, mockVscodeParam);
    const result7 = applyEdits(input7, edits7);
    console.log('Test 7:', result7 === expected7 ? 'PASS' : 'FAIL');
    if (result7 !== expected7) {
        console.log('Expected:', expected7);
        console.log('Got:', result7);
    }

    // Test 8: Empty lines and comments
    const input8 = `(module test
;; comment
(def a 1)

(def b 2)
)`;
    const expected8 = `(module test
    ;; comment
    (def a 1)

    (def b 2)
)`;
    const doc8 = new MockTextDocument(input8);
    const edits8 = formatGlueDocument(doc8 as any, mockVscodeParam);
    const result8 = applyEdits(input8, edits8);
    console.log('Test 8:', result8 === expected8 ? 'PASS' : 'FAIL');
    if (result8 !== expected8) {
        console.log('Expected:', expected8);
        console.log('Got:', result8);
    }

    // Test 9: Strings
    const input9 = `(module test
(def msg "hello world")
(def a 1)
)`;
    const expected9 = `(module test
    (def msg "hello world")
    (def a 1)
)`;
    const doc9 = new MockTextDocument(input9);
    const edits9 = formatGlueDocument(doc9 as any, mockVscodeParam);
    const result9 = applyEdits(input9, edits9);
    console.log('Test 9:', result9 === expected9 ? 'PASS' : 'FAIL');
    if (result9 !== expected9) {
        console.log('Expected:', expected9);
        console.log('Got:', result9);
    }

    // Test 10: Complex unbalanced
    const input10 = `(module test
(def a
(if (test)
(do-something)
(do-other)
)
)
)`;
    const expected10 = `(module test
    (def a
        (if (test)
            (do-something)
            (do-other)
        )
    )
)`;
    const doc10 = new MockTextDocument(input10);
    const edits10 = formatGlueDocument(doc10 as any, mockVscodeParam);
    const result10 = applyEdits(input10, edits10);
    console.log('Test 10:', result10 === expected10 ? 'PASS' : 'FAIL');
    if (result10 !== expected10) {
        console.log('Expected:', expected10);
        console.log('Got:', result10);
    }
}

function applyEdits(original: string, edits: any[]): string {
    let result = original.split('\n');
    for (const edit of edits) {
        const line = edit.range.start.line;
        result[line] = edit.newText;
    }
    return result.join('\n');
}

// Run tests
testFormatter();
