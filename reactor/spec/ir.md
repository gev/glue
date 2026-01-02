# Intermediate Representation (IR) Specification

## Overview

The Intermediate Representation (IR) is Reactor's execution-ready format that results from compiling Abstract Syntax Trees (AST). While AST focuses on syntactic structure, IR focuses on execution semantics, providing a unified representation for all Reactor values, functions, and data structures during runtime evaluation.

## IR Structure

The IR is defined by the following algebraic data type:

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

## IR Node Types

### Primitive Values

#### Number
Represents numeric values with arbitrary precision.

**Structure:** `Number Scientific`
**Purpose:** Store integers, decimals, and scientific notation

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

### Composite Values

#### List
Represents ordered sequences of IR values.

**Structure:** `List [IR m]`
**Purpose:** Store sequences, function arguments, and data collections
**Examples:**
- `List [Number 1.0, Number 2.0, Number 3.0]` (data sequence)
- `List [Symbol "+", Number 1.0, Number 2.0]` (function call)
- `List []` (empty list)

#### Object
Represents key-value mappings (dictionaries).

**Structure:** `Object (Map Text (IR m))`
**Purpose:** Store structured data with named properties
**Examples:**
- `Object [("name", String "Alice"), ("age", Number 30.0)]`
- `Object [("x", Number 1.0), ("y", Number 2.0)]`

#### Module
Represents collections of exported bindings.

**Structure:** `Module (Map Text (IR m))`
**Purpose:** Store module namespaces and exported values
**Examples:**
- `Module [("pi", Number 3.14159), ("cos", <native function>)]`
- `Module [("add", <closure>), ("multiply", <closure>)]`

### Executable Values

#### Native
Represents host language functions and special operations.

**Structure:** `Native (Native m)`
**Purpose:** Interface with host language implementations
**Variants:**
- `Func ([IR m] -> m (IR m))` - Functions returning IR values
- `Cmd ([IR m] -> m ())` - Commands with side effects
- `Special ([IR m] -> m (Maybe (IR m)))` - Special forms and control flow

#### Closure
Represents user-defined functions with captured environment.

**Structure:** `Closure [Text] (IR m) (Env m)`
**Purpose:** Store lambda functions with parameters, body, and lexical scope
**Components:**
- `[Text]` - parameter names
- `IR m` - function body expression
- `Env m` - captured environment

**Examples:**
- `Closure ["x"] (List [Symbol "+", Symbol "x", Number 1.0]) <env>` - (lambda (x) (+ x 1))

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

## Compilation from AST

### AST to IR Transformation

The compilation process transforms AST nodes into equivalent IR nodes:

#### Atomic AST Nodes
- `AST.String s` → `String s`
- `AST.Number n` → `Number n`
- `AST.Symbol s` → `Symbol s` (or `DottedSymbol parts` if contains dots)

#### Composite AST Nodes
- `AST.List xs` → `List (map compile xs)`
- `AST.Object props` → `Object (Map.fromList (map (second compile) props))`

### Symbol Resolution
During compilation, dotted symbols in AST are split and converted to DottedSymbol IR nodes:
- AST: `Symbol "obj.field"` → IR: `DottedSymbol ["obj", "field"]`

## IR Usage Patterns

### Data Representation
- **Numbers** for arithmetic and numeric data
- **Strings** for text content
- **Lists** for sequences and collections
- **Objects** for structured records

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

### AST (Abstract Syntax Tree)
- AST focuses on syntax structure
- IR focuses on execution semantics
- Compilation transforms AST → IR

### Environment
- IR closures reference environment frames
- Environment provides variable resolution
- Environment threading enables lexical scoping

### Evaluation
- IR is the input to the evaluation process
- Evaluation traverses IR structure
- Evaluation applies meaning to IR nodes

## Implementation Notes

### Generic Parameter
The `m` type parameter enables different evaluation monads (IO, Identity, etc.) for different execution contexts.

### Performance Considerations
- IR uses efficient data structures (Maps, Vectors)
- Lazy evaluation where appropriate
- Minimal boxing/unboxing overhead

### Extensibility
The IR design allows for future language features through:
- Additional Native function types
- Extended Object/Map capabilities
- New primitive types if needed

## Summary

The Reactor IR provides a comprehensive, unified representation for all language constructs during execution:

- **Primitives**: Numbers, strings, symbols for basic values
- **Composites**: Lists, objects, modules for complex data
- **Executables**: Closures, native functions for code execution
- **Metadata**: Environments and scoping information

This design enables Reactor's flexible evaluation model while maintaining type safety, performance, and extensibility.
