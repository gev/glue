# Parsing to AST Specification

## Overview

The parser transforms Glue source text into Abstract Syntax Tree (AST) representation. This process involves lexical analysis (tokenization) followed by syntactic analysis (grammar parsing), resulting in a structured tree that captures the syntactic relationships between program elements.

## Parser Architecture

### Two-Phase Process

1. **Lexical Analysis**: Convert source text into tokens
2. **Syntactic Analysis**: Parse tokens into AST according to grammar rules

### Parser Interface

```text
parse(source_text) → AST | ParserError
```

The parser produces a single AST node representing the entire parsed expression, or returns a `ParserError` if parsing fails.

### Error Types

The parser can produce the following error types:

```haskell
data ParserError where
    MixedContent Text      -- Property mixed with positional arguments
    UnpairedProperty Text  -- Property key without value
    SyntaxError Text       -- General syntax errors
```

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

The lexer follows the lexical rules defined in the [Syntax Specification](syntax.md), including number formats, string formats, and symbol formats.

## Syntactic Analysis

### Expression Grammar

The parser uses a recursive descent approach following the EBNF grammar defined in the [Syntax Specification](syntax.md). The parsing process handles:

- **Atomic expressions**: Numbers, Strings, and Symbols
- **Compound expressions**: Lists and Objects

### Parsing Precedence

Expressions are parsed in this order of priority (highest first):

1. **Lists and property objects** (`(expr...)`, `(:key val...)`)
2. **Atomic literals** (numbers, strings)
3. **Symbols** (identifiers, operators)

## AST Construction

### Atomic Values

#### Numbers
```text
Input: "42"
AST: Number 42
```

**Supported Number Formats:**
- **Integers**: Whole numbers like `42`, `-15`, `0`
- **Floating-point**: Decimal numbers like `3.14159`, `-273.15`
- **Scientific notation**: Exponential format like `1.23e-4`, `6.02e23`
- **Positive/Negative**: All number types support positive and negative values

#### Strings
```text
Input: "\"hello\""
```
```text
AST: String "hello"
```

**Escape Sequences:**
- `\"` → double quote character
- `\n` → newline character
- `\t` → tab character
- `\\` → backslash character
- `\"with \"quotes\"\"` → `with "quotes"`

#### Symbols
```text
Input: "variable"
```
```text
AST: Symbol "variable"
```

### Lists

#### Empty List
```text
Input: "()"
```
```text
AST: List []
```

#### Generic List
```text
Input: "(1 "a" foo)"
```
```text
AST: List [Number 1, String "a", Symbol "foo"]
```

#### Nested Lists
```text
Input: "((lambda (x) x) 5)"
```
```text
AST: List [
    List [Symbol "lambda", List [Symbol "x"], Symbol "x"],
    Number 5
]
```

#### Function Calls
```text
Input: "(+ 1 2)"
```
```text
AST: List [Symbol "+", Number 1, Number 2]
```

#### Function Call with Named Arguments (Syntax Sugar)
```text
Input: "(f :x 1 :y 2)"
```
```text
AST: List [Symbol "f", Object [("x", Number 1), ("y", Number 2)]]
```
A function call where the first element is the function name and subsequent elements can include named argument parsed as object.

### Objects

#### Empty Object
```text
Input: "(:)"
```
```text
AST: Object []
```

#### Simple Object
```text
Input: "(:name \"Alice\" :age 30)"
```
```text
AST: Object [
    ("name", String "Alice"),
    ("age", Number 30)
]
```

#### Nested Objects
```text
Input: "(:user (:name \"Bob\") :active true)"
```
```text
AST: Object [
    ("user", Object [("name", String "Bob")]),
    ("active", Symbol "true")
]
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
3. **Check** for object: if first token is `:`, parse object
4. **Otherwise**, parse expressions until closing `)`
5. **Return** `List` with collected elements

### Object Parsing

1. **Parse** colon-prefixed symbol as key
2. **Parse** following expression as value
3. **Repeat** until until closing `)`
4. **Return** `Object` with key-value pairs

### Error Handling

#### Syntax Errors

**Unmatched Parentheses:**
```text
Input: "(+ 1 2"
```
```text
Error: SyntaxError "Unexpected end of input"
```

**Invalid Number Format:**
```text
Input: "1.2.3"
```
```text
Error: SyntaxError "Multiple decimal points"
```

**Malformed String:**
```text
Input: "\"unclosed"
```
```text
Error: SyntaxError "Unterminated string literal"
```

**Mixed Content:**
```text
Input: "(:name \"Alice\" value)"
```
```text
Error: MixedContent "value"
```

**Unpaired Property:**
```text
Input: "(:name \"Alice\" :age)"
```
```text
Error: UnpairedProperty ":age"
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
- Regular lists vs objects
- Different atomic literal types

## Relationship to Other Components


### Syntax Specification
Parser implements the grammar defined in the [Syntax Specification](syntax.md).

### AST
Parser produces the AST that serves as input to compilation.  
See [AST Specification](ast.md) for detailed AST node structure.

### IR
Parsed AST is compiled to IR for execution.  
See [IR Specification](ir.md) for the target representation.
