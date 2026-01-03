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
    List :: [AST] -> AST
    Object :: [(Text, AST)] -> AST
```

## AST Node Types

### String
Represents string literal values in source code.

**Purpose:** Store textual data with escape sequence support

**Examples:**
- `"hello world"`
- `"with \"quotes\" and \n newlines"`

### Number
Represents numeric literal values (integers and floating-point numbers).

**Purpose:** Store numeric values using scientific notation

**Examples:**
- `42` (integer)
- `3.14159` (decimal)
- `1.23e-4` (scientific notation)

### Symbol
Represents identifiers, keywords, and operators.

**Purpose:** Store names and symbolic references

**Examples:**
- `x` (variable)
- `my-function` (function name)
- `+` (operator)
- `math.pi` (dotted path)

### List
Represents ordered sequences of expressions, including function calls, special forms, and data lists.

**Purpose:** Store sequential expressions and function applications

**Structure:** Ordered array of AST nodes

**Examples:**
- `(+)`
- `(+ 1 2 3)`
- `(if (> x 0) x (- x))`

### Object
Represents objects (key-value mappings).

**Purpose:** Store structured data with named properties

**Structure:** Array of key-value pairs

**Examples:**
- `(:name "Alice" :age 30)`
- `(:user (:name "Bob") :config (:theme "dark"))`

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
See [Syntax Specification](syntax.md) for source syntax definition.

### Parsing
AST is constructed by the parser from source text.  
See [Parsing to AST](parsing-to-ast.md) for construction process and algorithms.

### IR (Intermediate Representation)
AST focuses on syntax; IR focuses on semantics and execution. Compilation transforms AST to IR.  
See [AST to IR Compilation](compilation-ast-ir.md) for the transformation process and [IR Specification](ir.md) for the target execution representation.

## Summary

The AST provides Reactor's syntactic foundation, offering a clean tree representation that captures source code structure.
