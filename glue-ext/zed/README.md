# Glue support for Zed

This folder contains the Zed language extension and the Tree-sitter grammar source for Glue.

## Features

- `.glue` language registration and semicolon comments
- Tree-sitter highlighting for lists/calls, special forms, property keys, strings, numbers, booleans, and operators
- Lisp-style indentation rules
- A stdin/stdout formatter ported from `vscode/src/formatter.ts` (four-space indentation)

## Development status

Zed language support is declarative: a Rust/WASM extension binary is not needed for grammar, highlights, brackets, or indentation queries. Zed does not expose a formatter callback in its extension API; formatting is configured as an external command in Zed settings. `settings.example.json` shows the Glue formatter and format-on-save settings.

The extension manifest currently points to `https://github.com/gev/tree-sitter-glue`. Publish the included grammar source as that Git repository and replace `rev = "main"` with a pinned commit before installing the extension through Zed. For local development, Zed supports a `file://` grammar repository URL in `extension.toml`.

Install Node.js, then install the formatter CLI from the project root:

```sh
npm install --global ./glue-ext/zed/languages/glue
```

Merge `settings.example.json` into Zed's settings (`zed: open settings`) to enable format-on-save. You can also invoke **editor: format** manually. The CLI reads stdin and writes formatted source to stdout:

```sh
node glue-ext/zed/languages/glue/formatter.js < input.glue
```

The formatter follows the existing VS Code formatter's four-space, parenthesis-count indentation policy. Comments and string contents do not affect indentation.
