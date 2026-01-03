# Glue AST to IR Compilation

## Overview

Glue's compilation process transforms Abstract Syntax Tree (AST) nodes into Intermediate Representation (IR) nodes. This compilation step bridges the syntactic representation of source code with the runtime-evaluable form used by the interpreter.

## Data Types

### AST Data Types

```haskell
data AST where
    String :: Text -> AST
    Number :: Scientific -> AST
    Symbol :: Text -> AST
    AtomList :: [AST] -> AST
    PropList :: [(Text, AST)] -> AST
```

### IR Data Types

```haskell
data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Module (Map Text (IR m))
    | Native (Native m)
    | Closure [Text] (IR m) (Env m)
```

## Compilation Rules

### Primitive Values

#### Numbers
**AST:** `Number n`
**IR:** `Number n`
**Rule:** Numeric literals pass through unchanged

**Example:**
```closure
42
```
**AST:** `Number 42`
**IR:** `Number 42`

#### Strings
**AST:** `String s`
**IR:** `String s`
**Rule:** String literals pass through unchanged

**Example:**
```closure
"hello"
```
**AST:** `String "hello"`
**IR:** `String "hello"`

### Symbols and Identifiers

#### Simple Symbols
**AST:** `Symbol s` (where s contains no dots)
**IR:** `Symbol s`
**Rule:** Simple identifiers pass through unchanged

**Example:**
```closure
my-var
```
**AST:** `Symbol "my-var"`
**IR:** `Symbol "my-var"`

#### Dotted Symbols
**AST:** `Symbol s` (where s contains dots)
**IR:** `DottedSymbol parts` (split on ".")
**Rule:** Dot-separated identifiers become hierarchical symbol lists

**Example:**
```closure
math.pi
```
**AST:** `Symbol "math.pi"`
**IR:** `DottedSymbol ["math", "pi"]`

**Example:**
```closure
user.address.city
```
**AST:** `Symbol "user.address.city"`
**IR:** `DottedSymbol ["user", "address", "city"]`

### Composite Structures

#### Lists
**AST:** `AtomList elements`
**IR:** `List (map compile elements)`
**Rule:** List elements are recursively compiled

**Example:**
```closure
(+ 1 2)
```
**AST:** `AtomList [Symbol "+", Number 1, Number 2]`
**IR:** `List [Symbol "+", Number 1, Number 2]`

#### Property Objects
**AST:** `PropList properties`
**IR:** `Object (Map.fromList (map compile properties))`
**Rule:** Property key-value pairs become object map entries

**Example:**
```closure
(:name "Alice" :age 30)
```
**AST:** `PropList [("name", String "Alice"), ("age", Number 30)]`
**IR:** `Object (fromList [("name", String "Alice"), ("age", Number 30)])`

### Complex Examples

#### Function Call with Object
```closure
(foo :x 1 :y 2)
```
**AST:** `AtomList [Symbol "foo", PropList [("x", Number 1), ("y", Number 2)]]`
**IR:** `List [Symbol "foo", Object (fromList [("x", Number 1), ("y", Number 2)])]`

#### Nested Structures
```closure
(:user (:name "Bob") :data (1 2 3))
```
**AST:** `PropList [("user", PropList [("name", String "Bob")]), ("data", AtomList [Number 1, Number 2, Number 3])]`
**IR:** `Object (fromList [("user", Object (fromList [("name", String "Bob")])), ("data", List [Number 1, Number 2, Number 3])])`

## Compilation Pipeline

```
Source Code
    ↓ (parse)
AST
    ↓ (compile)
IR
    ↓ (evaluate)
Result
```

## Key Transformations

### Symbol Resolution
- **Input:** Flat dotted strings in AST
- **Output:** Hierarchical symbol lists in IR
- **Purpose:** Enable property access and namespace resolution

### Structure Preservation
- **Lists:** Maintain sequence and nesting
- **Objects:** Convert from association lists to efficient maps
- **Primitives:** Pass through with type preservation

### Recursive Processing
- **Depth-first:** All AST subtrees are recursively compiled
- **Type consistency:** Each AST node type maps to corresponding IR type
- **Structure integrity:** Nested structures maintain their relationships

## Compilation Invariants

### Type Preservation
- Numeric values remain as numbers
- Text values remain as strings
- Structural relationships are maintained

### Semantic Equivalence
- Compiled IR represents the same program semantics as source AST
- No information loss during compilation
- All source constructs have IR equivalents

### Deterministic Output
- Same AST always produces same IR
- Compilation is pure and side-effect free
- No external dependencies during compilation

## Summary

AST to IR compilation transforms Glue's syntactic representation into its runtime-evaluable form through systematic rules:

- **Primitives** pass through unchanged
- **Symbols** may split into hierarchical forms
- **Lists** recursively compile their elements
- **Objects** convert to efficient map structures

This compilation step enables the separation of parsing (syntax analysis) from evaluation (semantic execution), providing a clean architecture for the Glue language implementation.
