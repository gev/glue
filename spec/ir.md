# Intermediate Representation (IR) Specification

## Overview

The Intermediate Representation (IR) is Glue's execution-ready format that results from compiling [Abstract Syntax Trees (AST)](ast.md). While AST focuses on syntactic structure, IR focuses on execution semantics, providing a unified representation for all Glue values, functions, and data structures during runtime evaluation.

## IR Structure

The `IR` is defined by the following algebraic data type:

```haskell
data IR m
    = Integer Int
    | Float Double
    | String Text
    | Bool Bool
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Module (Map Text (IR m))
    | Void
    | Native (Native m)
    | Closure [Text] (IR m) (Env m)
```

## IR Node Types

### Primitive Values

#### Integer
Represents integer values.

**Structure:** `Integer Int`
**Purpose:** Store whole number values

#### Float
Represents floating-point values.

**Structure:** `Float Double`
**Purpose:** Store decimal and scientific notation values

#### Bool
Represents boolean values.

**Structure:** `Bool Bool`
**Purpose:** Store true/false values

#### String
Represents text values with escape sequence support.

**Structure:** `String Text`
**Purpose:** Store string literals and text data

#### Symbol
Represents simple identifiers and atomic names.

**Structure:** `Symbol Text`
**Purpose:** Store variable names, function names, and operators

#### DottedSymbol
Represents hierarchical identifiers with dot notation.

**Structure:** `DottedSymbol [Text]`
**Purpose:** Store property access paths and qualified names

#### Void
Represents unspecified values returned by operations that don't produce meaningful results.

**Structure:** `Void`
**Purpose:** Store void values from operations like variable definitions, assignments, and print statements
**Examples:**

- **Variable definition result**

    `Void` (result of `(def x 42)`)

- **Assignment result**

    `Void` (result of `(set x 100)`)

- **Print statement result**

    `Void` (result of `(print "hello")`)

### Composite Values

#### List

Represents ordered sequences of IR values.

**Structure:** `List [IR m]`
**Purpose:** Store sequences, function arguments, and data collections
**Examples:**

- **Empty list**

    `List []`

- **Data sequence**

    `List [Integer 1, Integer 2, Integer 3]`

- **Function call**

    `List [Symbol "+", Integer 1, Integer 2]`

#### Object

Represents key-value mappings (dictionaries).

**Structure:** `Object (Map Text (IR m))`

**Purpose:** Store structured data objects with named properties

**Examples:**

- **Simple object with primitive values**

    `Object [("name", String "Alice"), ("age", Integer 30)]`

- **Coordinate object**

  `Object [("x", Float 1.0), ("y", Float 2.0)]`

- **Nested object structure**

  `Object [("user", Object [("name", String "Alice"), ("profile", Object [("age", Integer 30), ("city", String "NYC")])])]`

#### Module

Represents hierarchical collections of exported bindings.

**Structure:** `Module (Map Text (IR m))`
**Purpose:** Store module namespaces and exported values

**Simple Modules:**
Modules can contain direct bindings to values, functions, and other modules.

**Examples:**

- **Simple module with constants and functions**

    `Module [("pi", Float 3.14159), ("cos", <native function>)]`

**Nested Modules:**
Modules can contain other modules, creating hierarchical namespaces.

**Examples:**

- **Module containing a submodule**

    `Module [("arithmetic", Module [("add", <closure>), ("multiply", <closure>)])]`

- **Deeply nested module hierarchy**

    `Module [("math", Module [("trig", Module [("sin", <native>), ("cos", <native>)]), ("constants", Module [("pi", Float 3.14159)])])]`

### Executable Values

#### Native

Represents host language functions and special operations.

**Structure:** `Native (Native m)`
**Purpose:** Interface with host language implementations
**Variants:**
- `Func ([IR m] -> m (IR m))` - Functions returning IR values
- `Special ([IR m] -> m (IR m))` - Special forms and control flow

#### Closure
Represents user-defined functions with captured environment.

**Structure:** `Closure [Text] (IR m) (Env m)`
**Purpose:** Store lambda functions with parameters, body, and lexical scope
**Components:**
- `[Text]` - parameter names
- `IR m` - function body expression
- `Env m` - captured environment

**Examples:**

- **Lambda function**

    `Closure ["x"] (List [Symbol "+", Symbol "x", Float 1.0]) <env>`

## IR Operations

### Type Predicates

#### List Operations

- `isList :: IR m -> Bool` - Check if IR node is a list
- `listLength :: IR m -> Int` - Get list length

#### Object Operations

- `isObject :: IR m -> Bool` - Check if IR node is an object
- `objectSize :: IR m -> Int` - Get number of properties
- `objectLookup :: Text -> IR m -> Maybe (IR m)` - Access property by name

#### Symbol Operations

- `isSymbol :: IR m -> Bool` - Check if IR node is a symbol
- `getSymbol :: IR m -> Text` - Extract symbol text (handles both Symbol and DottedSymbol)

### Serialization

IR nodes can be converted to human-readable string representations for debugging and display.

### Equality

IR nodes support structural equality comparison for testing and comparison operations.

## IR Usage Patterns

### Data Representation

- **Numbers** for arithmetic and numeric data
- **Strings** for text content
- **Lists** for sequences and collections
- **Objects** for structured records
- **Void** for unspecified operation results

### Code Representation

- **Symbols** for variable and function references
- **DottedSymbols** for property and module access
- **Lists** for function applications
- **Closures** for user-defined functions

### Runtime Integration

- **Native functions** for host language operations
- **Modules** for namespace management
- **Special forms** for control flow and macros

## IR Properties

### Composability

All IR node types can contain other IR nodes, enabling uniform representation of data and code.

### Type Safety

The IR type system prevents invalid combinations at compile time.

### Environment Threading

Closures capture lexical environments, enabling proper scoping and variable capture.

### Evaluation Ready

IR is optimized for runtime evaluation, with efficient representations for common operations.

## Relationship to Other Components

- [AST](ast.md) focuses on syntax structure
- Compilation transforms [AST](ast.md) → [IR](ir.md) (see [AST to IR Compilation](compilation-ast-ir.md))

### Evaluation

- IR is the input to the evaluation process
- Evaluation traverses IR structure
- Evaluation applies meaning to IR nodes

This design enables Glue's flexible evaluation model while maintaining type safety, performance, and extensibility.
