export function formatGlueDocument(document: any, vscode: any): any[] {
    const edits: any[] = [];
    const indentSize = 4; // 4 spaces

    let currentIndent = 0;
    for (let i = 0; i < document.lineCount; i++) {
        const line = document.lineAt(i);
        let text = line.text.trim();

        // Handle empty lines
        if (text === '') {
            // Keep empty lines as is
            continue;
        }

        // Remove spaces after opening parentheses and before closing parentheses
        text = text.replace(/\(\s+/g, '(').replace(/\s+\)/g, ')');

        const openCount = (text.match(/\(/g) || []).length;
        const closeCount = (text.match(/\)/g) || []).length;

        let lineIndent = currentIndent;
        if (text.startsWith(')')) {
            // Closing line, align with the opening of the form
            lineIndent = Math.max(0, currentIndent - 1);
        }
        // Adjust currentIndent based on net parens
        currentIndent += openCount - closeCount;

        const newIndent = ' '.repeat(lineIndent * indentSize);
        if (line.text !== newIndent + text) {
            const range = new vscode.Range(line.range.start, line.range.end);
            edits.push(vscode.TextEdit.replace(range, newIndent + text));
        }
    }

    return edits;
}
