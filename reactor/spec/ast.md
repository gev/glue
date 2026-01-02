# Abstract Syntax Tree (AST) Specification

## Overview

The Abstract Syntax Tree (AST) is Reactor's intermediate representation of program syntax after parsing. It provides a structured, tree-based representation of source code that captures the syntactic relationships between program elements while discarding superficial details like whitespace and comments.

## AST Data Structure

The AST is defined by the following algebraic data type:

```haskell
data AST where
    String :: Text -> AST
    Number :: Scientific -> AST
    Symbol :: Text -> AST
    AtomList :: [AST] -> AST
    PropList :: [(Text, AST)] -> AST
```

## AST Node Types

### String
Represents string literal values in source code.

**Syntax:** `"text content"`
**Purpose:** Store textual data with escape sequence support
**Examples:**
- `"hello world"`
- `"with \"quotes\" and \n newlines"`

### Number
Represents numeric literal values (integers and floating-point numbers).

**Syntax:** `42`, `3.14159`, `-273.15`, `1.23e-4`
**Purpose:** Store numeric values using scientific notation
**Examples:**
- `42` (integer)
- `3.14159` (decimal)
- `1.23e-4` (scientific notation)

### Symbol
Represents identifiers, keywords, and operators.

**Syntax:** `identifier`, `my-variable`, `+`, `obj.field`
**Purpose:** Store names and symbolic references
**Validation:** Must start with letter, may contain letters, digits, and special characters
**Examples:**
- `x` (variable)
- `my-function` (function name)
- `+` (operator)
- `math.pi` (dotted path)

### AtomList
Represents ordered sequences of expressions, including function calls, special forms, and data lists.

**Syntax:** `(expr1 expr2 expr3 ...)`
**Purpose:** Store sequential expressions and function applications
**Structure:** Ordered array of AST nodes
**Examples:**
- `(+)`
- `(+ 1 2 3)`
- `(if (> x 0) x (- x))`

### PropList
Represents property objects (key-value mappings).

**Syntax:** `(:key1 value1 :key2 value2 ...)`
**Purpose:** Store structured data with named properties
**Structure:** Array of key-value pairs where keys are strings
**Validation:** Keys must be prefixed with `:`, each key must have a value
**Examples:**
- `(:name "Alice" :age 30)`
- `(:user (:name "Bob") :config (:theme "dark"))`

## Parsing Rules

### Expression Precedence

Expressions are parsed in the following order of priority (highest first):

1. Atom lists and property lists (`(expr...)`, `(:key val...)`)
2. String literals (`"text"`)
3. Numeric literals (`42`, `3.14`)
4. Symbols (`x`, `+`, `obj.field`)

### Lexical Rules

#### Whitespace
Spaces, tabs, and newlines are ignored except within string literals.

#### Comments
- Line comments: `;` followed by text until end of line
- Block comments: `#|` followed by text until `|#`

#### Numbers
- Support integer, decimal, and scientific notation
- Invalid: multiple decimal points, leading zeros (except `0`), non-numeric characters

#### Strings
- Enclosed in double quotes
- Support escape sequences: `\"`, `\n`, `\t`, `\\`
- Invalid: unclosed strings, invalid escape sequences

#### Symbols
- Start with letter or special character
- May contain: letters, digits, `-`, `.`, `_`, `:`, `!`, `?`, `\`, `=`, `>`, `<`, `/`, `*`, `+`, `%`
- Invalid: starting with digit, containing spaces

### Structural Rules

#### Atom lists
- Parenthesized expressions create ordered sequences
- Empty lists `()` are valid
- Nested lists are supported
- Mixed content (properties + positional args) is invalid

#### Property lists
- Start with `:`-prefixed symbol
- Keys must be unique within the object
- Values can be any expression type
- Cannot be mixed with atom lists in the same list

## AST Construction

### Parser Output
The parser produces a single AST node representing the entire parsed expression.

## AST Operations

### Serialization
AST nodes can be converted back to source-like text representation.

### Equality
AST nodes support structural equality comparison.

### Traversal
AST trees can be traversed and transformed during compilation phases.

## Relationship to Other Representations

### Source Text
AST discards superficial details (whitespace, comments) while preserving syntactic structure.

### IR (Intermediate Representation)
AST focuses on syntax; IR focuses on semantics and execution. Compilation transforms AST to IR.

## Summary

The AST provides Reactor's syntactic foundation, offering a clean tree representation that captures source code structure.
