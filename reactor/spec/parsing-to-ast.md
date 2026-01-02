# Parsing to AST Specification

## Overview

The parser transforms Reactor source text into Abstract Syntax Tree (AST) representation. This process involves lexical analysis (tokenization) followed by syntactic analysis (grammar parsing), resulting in a structured tree that captures the syntactic relationships between program elements.

## Parser Architecture

### Two-Phase Process

1. **Lexical Analysis**: Convert source text into tokens
2. **Syntactic Analysis**: Parse tokens into AST according to grammar rules

### Parser Interface

```
parse(source_text) → AST | Error
```

The parser produces a single AST node representing the entire parsed expression, or returns an error if parsing fails.

## Lexical Analysis

### Token Types

The lexer recognizes the following token categories:

#### Whitespace
- Spaces, tabs, newlines
- Ignored except within string literals

#### Comments
- Line comments: `;` followed by text until end of line
- Block comments: `#|` followed by text until `|#`

#### Literals
- **Numbers**: Integer, decimal, and scientific notation
- **Strings**: Double-quoted text with escape sequences
- **Symbols**: Identifiers and operators

#### Punctuation
- Parentheses: `(`, `)`
- Quote prefix: `'`

### Lexical Rules

#### Numbers
```
number ::= digits ["." digits] [("e"|"E") ["+"|"-"] digits]
digits ::= digit+
digit  ::= "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
```

**Examples:**
- `42` → integer literal
- `3.14159` → decimal literal
- `1.23e-4` → scientific notation

#### Strings
```
string ::= '"' (char | escape)* '"'
escape ::= "\" ("\"" | "n" | "t" | "\" | ...)
```

**Examples:**
- `"hello"` → simple string
- `"with \"quotes\""` → escaped quotes
- `"line1\nline2"` → multiline string

#### Symbols
```
symbol ::= letter (letter | digit | special)*
special ::= "-" | "_" | "." | "!" | "?" | "\" | "=" | ">" | "<" | "/" | "*" | "+" | "%" | "@" | "#" | "$"
```

**Examples:**
- `variable` → identifier
- `my-function` → function name
- `+` → operator
- `obj.field` → dotted path

## Syntactic Analysis

### Expression Grammar

The parser uses a recursive descent approach following the grammar defined in the [Syntax Specification](syntax.md). The parsing process handles:

- **Atomic expressions**: Numbers, strings, and symbols
- **Compound expressions**: Lists and property objects
- **Quoted expressions**: Data literals using the `'` prefix

For the complete EBNF grammar definition, see [Syntax Specification](syntax.md).

### Parsing Precedence

Expressions are parsed in this order of priority (highest first):

1. **Quoted expressions** (`'expr`)
2. **Lists and property objects** (`(expr...)`, `(:key val...)`)
3. **Atomic literals** (numbers, strings)
4. **Symbols** (identifiers, operators)

## AST Construction

### Atomic Values

#### Numbers
```
Input: "42"
AST: Number 42
```

#### Strings
```
Input: "\"hello\""
AST: String "hello"
```

#### Symbols
```
Input: "variable"
AST: Symbol "variable"
```

### Lists

#### Function Calls
```
Input: "(+ 1 2)"
AST: List [Symbol "+", Number 1, Number 2]
```

#### Nested Lists
```
Input: "((lambda (x) x) 5)"
AST: List [
    List [Symbol "lambda", List [Symbol "x"], Symbol "x"],
    Number 5
]
```

#### Empty Lists
```
Input: "()"
AST: List []
```

### Property Objects

#### Simple Objects
```
Input: "(:name \"Alice\" :age 30)"
AST: Object [
    ("name", String "Alice"),
    ("age", Number 30)
]
```

#### Nested Objects
```
Input: "(:user (:name \"Bob\") :active true)"
AST: Object [
    ("user", Object [("name", String "Bob")]),
    ("active", Symbol "true")
]
```

#### Empty Objects
```
Input: "(:)"
AST: Object []
```

### Quoted Expressions

#### Simple Quoting
```
Input: "'x"
AST: List [Symbol "quote", Symbol "x"]
```

#### Complex Quoting
```
Input: "'(+ 1 2)"
AST: List [Symbol "quote", List [Symbol "+", Number 1, Number 2]]
```

#### Nested Quoting
```
Input: "''foo"
AST: List [Symbol "quote", List [Symbol "quote", Symbol "foo"]]
```

## Parsing Algorithm

### Top-Level Parsing

1. **Tokenize** input into lexical tokens
2. **Parse expression** starting from top level
3. **Validate** no remaining unparsed tokens
4. **Return** constructed AST

### List Parsing

1. **Consume** opening parenthesis `(`
2. **Check** for empty list: if immediate `)`, return `List []`
3. **Check** for property list: if first token is `:`, parse property pairs
4. **Otherwise**, parse expressions until closing `)`
5. **Return** `List` with collected elements

### Property List Parsing

1. **Parse** colon-prefixed symbol as key
2. **Parse** following expression as value
3. **Repeat** until no more `:symbol` patterns
4. **Return** `Object` with key-value pairs

### Error Handling

#### Syntax Errors

**Unmatched Parentheses:**
```
Input: "(+ 1 2"
Error: Unexpected end of input
```

**Invalid Number Format:**
```
Input: "1.2.3"
Error: Multiple decimal points
```

**Malformed String:**
```
Input: "\"unclosed"
Error: Unterminated string literal
```

**Mixed Content:**
```
Input: "(:name \"Alice\" value)"
Error: Cannot mix properties and positional arguments
```

**Unpaired Property:**
```
Input: "(:name \"Alice\" :age)"
Error: Property ':age' requires a value
```

#### Error Recovery

The parser provides detailed error messages including:
- Error location in source text
- Clear description of the problem
- Suggestions for valid syntax where applicable

## Implementation Notes

### Parser State

The parser maintains minimal state:
- Current token stream
- Error context for reporting

### Backtracking

The parser uses lookahead to distinguish between:
- Regular lists vs property lists
- Different atomic literal types

### Performance Characteristics

- **Linear time**: O(n) in input size
- **Single pass**: No backtracking for ambiguity resolution
- **Memory efficient**: AST construction during parsing

## Relationship to Other Components

### Source Text
Parser discards superficial details (whitespace, comments) while preserving syntactic structure.

### AST
Parser produces the AST that serves as input to compilation.

### Lexer Integration
Lexical analysis is integrated with parsing for efficiency.

## Summary

The Reactor parser provides a robust, efficient transformation from human-readable source text to machine-processable AST. Its design emphasizes:

- **Clarity**: Direct correspondence between syntax and AST structure
- **Robustness**: Comprehensive error reporting and validation
- **Efficiency**: Linear-time parsing with minimal memory overhead
- **Extensibility**: Clear separation of lexical and syntactic analysis
