import * as vscode from 'vscode';
import { formatGlueDocument } from './formatter';
import { GlueSemanticTokensProvider } from './semantic';

// Make vscode available globally for the formatter
(globalThis as any).vscode = vscode;

export function activate(context: vscode.ExtensionContext) {
    console.log('Glue extension activated');
    // Register the document formatting provider for Glue
    const provider = vscode.languages.registerDocumentFormattingEditProvider('glue', {
        provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.TextEdit[] {
            console.log('Glue formatter called for document:', document.fileName);
            try {
                const edits = formatGlueDocument(document, vscode);
                console.log('Glue formatter returning', edits.length, 'edits');
                return edits;
            } catch (error) {
                console.error('Glue formatter error:', error);
                return [];
            }
        }
    });

    context.subscriptions.push(provider);

    // Register semantic tokens provider
    // const legend = new vscode.SemanticTokensLegend(['function', 'variable', 'keyword', 'property'], []);
    // const semanticProvider = vscode.languages.registerDocumentSemanticTokensProvider('glue', new GlueSemanticTokensProvider(legend), legend);

    // context.subscriptions.push(semanticProvider);
}

export function deactivate() { }
