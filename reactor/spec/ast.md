# Abstract Syntax Tree (AST) Specification

## Overview

The Abstract Syntax Tree (AST) is Reactor's intermediate representation of program syntax after parsing. It provides a structured, tree-based representation of source code that captures the syntactic relationships between program elements while discarding superficial details like whitespace and comments.

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

1. Quoted expressions (`'expr`)
2. Lists and property objects (`(expr...)`, `(:key val...)`)
3. String literals (`"text"`)
4. Numeric literals (`42`, `3.14`)
5. Symbols (`x`, `+`, `obj.field`)

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

#### Lists
- Parenthesized expressions create ordered sequences
- Empty lists `()` are valid
- Nested lists are supported
- Mixed content (properties + positional args) is invalid

#### Property Objects
- Start with `:`-prefixed symbol
- Keys must be unique within the object
- Values can be any expression type
- Cannot be mixed with positional arguments in the same list

#### Quoted Expressions
- `'` prefix transforms any expression into `(quote expression)`
- Supports multiple levels: `''expr` → `(quote (quote expr))`

## AST Construction

### Parser Output
The parser produces a single AST node representing the entire parsed expression.

### Transformation Examples

#### Simple Function Call
```reactor
(+ 1 2)
```
**AST Structure:**
```
AtomList [
    Symbol "+",
    Number 1,
    Number 2
]
```

#### Property Object
```reactor
(:name "Alice" :age 30)
```
**AST Structure:**
```
PropList [
    ("name", String "Alice"),
    ("age", Number 30)
]
```

#### Nested Expressions
```reactor
(if (> x 0) (* x 2) 0)
```
**AST Structure:**
```
AtomList [
    Symbol "if",
    AtomList [Symbol ">", Symbol "x", Number 0],
    AtomList [Symbol "*", Symbol "x", Number 2],
    Number 0
]
```

## Error Conditions

### Syntax Errors

#### Mixed Content Error
**Condition:** Attempting to mix property syntax with positional arguments
**Example:** `(+ :x 1 2)` - invalid
**Valid Alternatives:**
- `(+ 1 2)` - positional only
- `(:x 1 :y 2)` - properties only

#### Unpaired Property Error
**Condition:** Property key without corresponding value
**Example:** `(:name "Alice" :age)` - invalid
**Valid:** `(:name "Alice" :age 30)`

#### General Syntax Errors
- Unmatched parentheses
- Invalid number formats
- Malformed strings
- Unexpected tokens

### Error Reporting
Parser provides detailed error messages including:
- Error location in source text
- Clear description of the problem
- Suggestions for valid syntax

## AST Properties

### Well-Formedness
Valid AST nodes maintain these invariants:
- Symbol names contain only valid characters
- Lists have proper nesting and balancing
- Property keys are valid strings
- Numeric values are well-formed

### Type Safety
The AST type system prevents invalid combinations at compile time.

## AST Operations

### Serialization
AST nodes can be converted back to source-like text representation for debugging and inspection.

### Equality
AST nodes support structural equality comparison.

### Traversal
AST trees can be traversed and transformed during compilation phases.

## Relationship to Other Representations

### Source Text
AST discards superficial details (whitespace, comments) while preserving syntactic structure.

### IR (Intermediate Representation)
AST focuses on syntax; IR focuses on semantics and execution. Compilation transforms AST to IR.

### Evaluation
AST is static representation; evaluation applies meaning to AST structures.

## Implementation Notes

### Parser Interface
```
parse(source_text) → AST | Error
```

### AST Node Interface
All AST nodes support:
- Type identification
- Structural equality
- Text serialization
- Tree traversal

## Summary

The AST provides Reactor's syntactic foundation, offering a clean tree representation that captures source code structure while enabling efficient compilation and analysis. Its design balances simplicity with expressiveness, supporting both data manipulation and program execution semantics.
