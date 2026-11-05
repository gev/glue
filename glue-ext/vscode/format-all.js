const fs = require('fs');
const path = require('path');

// Mock vscode for standalone use
const mockVscode = {
    Range: class {
        constructor(start, end) {
            this.start = start;
            this.end = end;
        }
    },
    TextEdit: {
        replace(range, newText) {
            return { range, newText };
        }
    }
};

global.vscode = mockVscode;

// Inline the formatGlueDocument function
function formatGlueDocument(document, vscode) {
    const edits = [];
    const indentSize = 4; // 4 spaces

    let currentIndent = 0;
    for (let i = 0; i < document.lineCount; i++) {
        const line = document.lineAt(i);
        let text = line.text.trim();

        // Remove spaces after opening parentheses and before closing parentheses
        text = text.replace(/\(\s+/g, '(').replace(/\s+\)/g, ')');

        const openCount = (text.match(/\(/g) || []).length;
        const closeCount = (text.match(/\)/g) || []).length;

        let lineIndent = currentIndent;
        if (openCount > closeCount) {
            currentIndent += 1;
        } else if (openCount < closeCount) {
            lineIndent = currentIndent - 1;
            currentIndent -= 1;
        }

        const newIndent = ' '.repeat(lineIndent * indentSize);
        if (line.text !== newIndent + text) {
            const range = new vscode.Range(line.range.start, line.range.end);
            edits.push(vscode.TextEdit.replace(range, newIndent + text));
        }
    }

    return edits;
}

// Mock document class
class MockDocument {
    constructor(content) {
        this.lines = content.split('\n');
    }

    get lineCount() {
        return this.lines.length;
    }

    lineAt(index) {
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

function applyEdits(original, edits) {
    let result = original.split('\n');
    for (const edit of edits) {
        const line = edit.range.start.line;
        result[line] = edit.newText;
    }
    return result.join('\n');
}

function formatFile(filePath) {
    const content = fs.readFileSync(filePath, 'utf8');
    const doc = new MockDocument(content);
    const edits = formatGlueDocument(doc, global.vscode);
    if (edits.length > 0) {
        const newContent = applyEdits(content, edits);
        fs.writeFileSync(filePath, newContent);
        console.log(`Formatted ${filePath}`);
    } else {
        console.log(`No changes needed for ${filePath}`);
    }
}

function formatAllGlueFiles(dir) {
    const items = fs.readdirSync(dir);
    for (const item of items) {
        const fullPath = path.join(dir, item);
        const stat = fs.statSync(fullPath);
        if (stat.isDirectory()) {
            formatAllGlueFiles(fullPath);
        } else if (item.endsWith('.glue')) {
            formatFile(fullPath);
        }
    }
}

// Test the formatter
const testInput = `(module test
(def a
(lambda (x)
(if x
1
0)
)
)
)
)`;
const doc = new MockDocument(testInput);
const edits = formatGlueDocument(doc, global.vscode);
const result = applyEdits(testInput, edits);
console.log('Input:');
console.log(testInput);
console.log('Output:');
console.log(result);

// Format all .glue files in the glue directory
// formatAllGlueFiles(path.join(__dirname, '..', 'glue'));
