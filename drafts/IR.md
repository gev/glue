# Glue Intermediate Representation (IR)

## Overview

Glue's Intermediate Representation (IR) is the core data structure used for representing Glue programs during execution. The IR provides a unified way to handle values, functions, objects, and modules in the Glue runtime system.

## IR Data Types

### Core IR Structure

```haskell
data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Module (Map Text (IR m))
    | NativeValue HostValue
    | NativeFunc ([IR m] -> m (IR m))
    | Special ([IR m] -> m (IR m))
    | Closure [Text] (IR m) (Env m)
```

### IR Node Types

#### `Number`
Represents numeric values using arbitrary-precision arithmetic.

**Structure:** `Number Scientific`
**Purpose:** Store integers, decimals, and scientific notation numbers
**Examples:**
- `Number 42.0` - integer 42
- `Number 3.14159` - decimal π approximation
- `Number 6.02e+23` - scientific notation

#### `String`
Represents text values.

**Structure:** `String Text`
**Purpose:** Store string literals and text data
**Examples:**
- `String "hello world"`
- `String "with \"quotes\" and \n newlines"`

#### `Symbol`
Represents simple identifiers and atomic symbols.

**Structure:** `Symbol Text`
**Purpose:** Store variable names, function names, and simple identifiers
**Examples:**
- `Symbol "x"` - variable x
- `Symbol "+"` - addition operator
- `Symbol "my-function"` - function name

#### `DottedSymbol`
Represents hierarchical identifiers with dot notation.

**Structure:** `DottedSymbol [Text]`
**Purpose:** Store property access paths and nested identifiers
**Examples:**
- `DottedSymbol ["obj", "field"]` - object.field
- `DottedSymbol ["math", "pi"]` - math.pi
- `DottedSymbol ["user", "address", "city"]` - user.address.city

#### `List`
Represents ordered collections of IR values.

**Structure:** `List [IR m]`
**Purpose:** Store sequences, function arguments, and data lists
**Examples:**
- `List [Number 1.0, Number 2.0, Number 3.0]` - [1, 2, 3]
- `List [Symbol "+", Number 1.0, Number 2.0]` - (+ 1 2)
- `List []` - empty list

#### `Object`
Represents key-value mappings (dictionaries/maps).

**Structure:** `Object (Map Text (IR m))`
**Purpose:** Store structured data with named properties
**Examples:**
- `Object (fromList [("name", String "Alice"), ("age", Number 30.0)])`
- `Object (fromList [("x", Number 1.0), ("y", Number 2.0)])`

#### `Module`
Represents collections of exported values and functions.

**Structure:** `Module (Map Text (IR m))`
**Purpose:** Store module namespaces and exported bindings
**Examples:**
- `Module (fromList [("pi", Number 3.14159), ("cos", <native function>)])`
- `Module (fromList [("add", <closure>), ("multiply", <closure>)])`

#### `NativeValue`
Represents host language objects and literals.

**Structure:** `NativeValue HostValue`
**Purpose:** Store host language values that don't need evaluation

#### `NativeFunc`
Represents host language functions.

**Structure:** `NativeFunc ([IR m] -> m (IR m))`
**Purpose:** Interface with host language functions that return IR values

#### `Special`
Represents special forms and control flow operations.

**Structure:** `Special ([IR m] -> m (IR m))`
**Purpose:** Handle special forms like `if`, `def`, `lambda` with custom evaluation rules

#### `Closure`
Represents user-defined functions with captured environment.

**Structure:** `Closure [Text] (IR m) (Env m)`
**Purpose:** Store lambda functions with parameter names, body, and captured scope
**Components:**
- `[Text]` - parameter names
- `IR m` - function body expression
- `Env m` - captured lexical environment

**Examples:**
- `Closure ["x"] (List [Symbol "+", Symbol "x", Number 1.0]) <env>` - (lambda (x) (+ x 1))



## IR Operations

### Type Predicates

#### List Operations

**Purpose:** Check if IR node is a list and get its length

#### Object Operations

**Purpose:** Work with object properties and size

#### Symbol Operations

**Purpose:** Check symbol types and extract symbol text

## IR Show Instance

**Purpose:** Convert IR to readable string representation
**Behavior:**
- Numbers: Standard numeric display
- Strings: Quoted with escaped characters
- Symbols: Raw text
- DottedSymbols: Dot-separated parts
- Lists: Parenthesized space-separated elements
- Complex types: Descriptive placeholders

## IR Equality

**Purpose:** Structural equality comparison between IR nodes
**Behavior:**
- Compares primitive values directly
- Compares collections structurally
- Treats functions/closures as unequal (not comparable)

### Composability
- All IR nodes can contain other IR nodes
- Uniform representation of data and code
- Environment threading enables closures and scoping

## IR Usage Patterns

### Data Representation
- Numbers for arithmetic
- Strings for text
- Lists for sequences
- Objects for structured data

### Code Representation
- Symbols for identifiers
- DottedSymbols for property access
- Lists for function calls
- Closures for user functions

### Runtime Integration
- Native functions for host language interface
- Modules for namespace management
- Objects for dynamic property storage

### Control Flow
- Special forms for conditional execution
- Closures for function abstraction
- Environment for lexical scoping

## Summary

The Glue IR provides a comprehensive and flexible representation for all Glue language constructs:

- **Primitives:** Numbers, strings, symbols
- **Composites:** Lists, objects, modules
- **Executables:** Closures, native functions, special forms
- **Metadata:** Environments, frames, scoping information

This unified representation enables Glue's flexible evaluation model while maintaining type safety and performance.
