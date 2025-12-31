# Reactor Intermediate Representation (IR)

## Overview

Reactor's Intermediate Representation (IR) is the core data structure used for representing Reactor programs during execution. The IR provides a unified way to handle values, functions, objects, and modules in the Reactor runtime system.

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
    | Native (Native m)
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

#### `Native`
Represents host language functions and operations.

**Structure:** `Native (Native m)`
**Purpose:** Interface with host language functions and special forms
**Subtypes:**
- `Func ([IR m] -> m (IR m))` - Regular functions returning IR values
- `Cmd ([IR m] -> m ())` - Commands with side effects (no return value)
- `Special ([IR m] -> m (Maybe (IR m)))` - Special forms (macros, control flow)

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

## Environment Types

### Frame
```haskell
type Frame m = Map.Map Text (IR m)
```

**Purpose:** Single scope level containing variable bindings
**Structure:** Map from symbol names to IR values
**Examples:**
- `fromList [("x", Number 42.0), ("y", String "hello")]`

### Environment
```haskell
type Env m = [Frame m]
```

**Purpose:** Stack of frames representing lexical scope
**Structure:** List of frames (innermost first)
**Examples:**
- `[globalFrame, functionFrame, blockFrame]` - nested scopes

## IR Operations

### Type Predicates

#### List Operations
```haskell
isList :: IR m -> Bool
listLength :: IR m -> Int
```

**Purpose:** Check if IR node is a list and get its length

#### Object Operations
```haskell
isObject :: IR m -> Bool
objectSize :: IR m -> Int
objectLookup :: Text -> IR m -> Maybe (IR m)
```

**Purpose:** Work with object properties and size

#### Symbol Operations
```haskell
isSymbol :: IR m -> Bool
getSymbol :: IR m -> Text
```

**Purpose:** Check symbol types and extract symbol text

## IR Show Instance

```haskell
instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Symbol s -> T.unpack s
        DottedSymbol parts -> T.unpack (T.intercalate "." parts)
        List xs -> "(" <> unwords (map show xs) <> ")"
        Object _ -> "{object}"
        Module _ -> "{module}"
        Native _ -> "<native>"
        Closure{} -> "<closure>"
```

**Purpose:** Convert IR to readable string representation
**Behavior:**
- Numbers: Standard numeric display
- Strings: Quoted with escaped characters
- Symbols: Raw text
- DottedSymbols: Dot-separated parts
- Lists: Parenthesized space-separated elements
- Complex types: Descriptive placeholders

## IR Equality

```haskell
instance Eq (IR m) where
    (Number a) == (Number b) = a == b
    (String a) == (String b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (DottedSymbol a) == (DottedSymbol b) = a == b
    (List a) == (List b) = a == b
    (Object a) == (Object b) = a == b
    (Module a) == (Module b) = a == b
    _ == _ = False
```

**Purpose:** Structural equality comparison between IR nodes
**Behavior:**
- Compares primitive values directly
- Compares collections structurally
- Treats functions/closures as unequal (not comparable)

## IR Design Principles

### Type Safety
- Strong typing through Haskell's type system
- Parametric polymorphism with effect type `m`
- Safe representation of all Reactor values

### Extensibility
- Open union type for adding new IR nodes
- Generic operations work with all IR types
- Environment abstraction supports different evaluation strategies

### Performance
- Efficient Map-based objects and modules
- Lazy evaluation support through effect type
- Minimal boxing/unboxing of primitive values

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

The Reactor IR provides a comprehensive and flexible representation for all Reactor language constructs:

- **Primitives:** Numbers, strings, symbols
- **Composites:** Lists, objects, modules
- **Executables:** Closures, native functions, special forms
- **Metadata:** Environments, frames, scoping information

This unified representation enables Reactor's flexible evaluation model while maintaining type safety and performance.
