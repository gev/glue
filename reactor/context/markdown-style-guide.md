# Markdown Style Guide for Reactor Specifications

This document provides guidelines for consistent markdown formatting and syntax highlighting in Reactor specification documents.

## Code Block Syntax Highlighting

Use the following language identifiers for code blocks:

### Primary Languages
- **`closure`** - Reactor language code examples
- **`haskell`** - Haskell implementation code
- **`ebnf`** - EBNF grammar specifications

### Data Formats
- **`json`** - JSON data structures and configuration
- **`text`** - Plain text, pseudocode, directory structures, or other examples

### Examples

#### Reactor Code
```closure
;; Define a function
(def add (lambda (a b) (+ a b)))

;; Use the function
(add 5 3)  ;; → 8
```

#### Haskell Implementation
```haskell
data AST where
    Symbol :: Text -> AST
    Number :: Scientific -> AST
    List :: [AST] -> AST
```

#### EBNF Grammar
```ebnf
program         ::= expr
expr            ::= atom | list | prop_list
atom            ::= number | string | symbol
```

#### JSON Configuration
```json
{
  "debug": false,
  "database": {
    "type": "postgres",
    "name": "myapp"
  }
}
```

#### Directory Structure
```text
stdlib/
├── core/
│   ├── list.rct      # (module core.list ...)
│   └── math.rct      # (module core.math ...)
└── utils/
    └── string.rct    # (module utils.string ...)
```

## General Markdown Guidelines

- Use descriptive headers with proper hierarchy
- Include cross-references to related documents
- Use consistent terminology throughout specifications
- Provide clear examples with comments where helpful
- Maintain technical precision and implementation focus
