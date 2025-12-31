# Reactor Abstract Syntax Tree (AST)

## Overview

Reactor's Abstract Syntax Tree (AST) represents the syntactic structure of Reactor programs after parsing. The AST is the first intermediate representation in Reactor's compilation pipeline, designed for simplicity and direct correspondence to source code syntax.

## AST Structure

### Core Data Types

```haskell
data AST where
    String :: Text -> AST
    Number :: Scientific -> AST
    Symbol :: Text -> AST
    AtomList :: [AST] -> AST
    PropList :: [(Text, AST)] -> AST
```

### AST Node Types

#### `String`
Represents string literals in source code.

**Syntax:** `"hello world"`
**AST:** `String "hello world"`

**Examples:**
```reactor
"simple string"
"with \"quotes\" and \n escapes"
```

#### `Number`
Represents numeric literals (integers and floats).

**Syntax:** `42`, `3.14159`, `-273.15`
**AST:** `Number 42`, `Number 3.14159`

**Examples:**
```reactor
42
-15
3.14159
1.23e-4
```

#### `Symbol`
Represents identifiers, keywords, and operators.

**Syntax:** `x`, `my-function`, `+`, `math.pi`
**AST:** `Symbol "x"`, `Symbol "+"`

**Special Cases:**
- Dotted symbols like `obj.field` are initially parsed as `Symbol "obj.field"`
- During compilation, these become `DottedSymbol ["obj", "field"]` in IR

**Examples:**
```reactor
x
my-variable
+
math
obj.field  ;; Initially: Symbol "obj.field"
```

#### `AtomList`
Represents function calls, special forms, and data lists.

**Syntax:** `(expr1 expr2 expr3)`
**AST:** `AtomList [expr1, expr2, expr3]`

**Examples:**
```reactor
(+ 1 2 3)           ;; Function call
(if (> x 0) x (- x)) ;; Special form
'(1 2 3)            ;; Quoted list
(list a b c)        ;; Function call
```

#### `PropList`
Represents property objects (dictionaries/maps).

**Syntax:** `(:key1 value1 :key2 value2)`
**AST:** `PropList [("key1", value1), ("key2", value2)]`

**Examples:**
```reactor
(:name "Alice" :age 30)
(:)
(:x (+ 1 2) :y (* 3 4))
```

## Compilation to IR

The AST is compiled to Intermediate Representation (IR) through the `compile` function:

```haskell
compile :: AST -> IR m
```

### Compilation Rules

| AST Node | IR Result | Notes |
|----------|-----------|-------|
| `String s` | `String s` | Direct mapping |
| `Number n` | `Number n` | Direct mapping |
| `Symbol s` | `Symbol s` or `DottedSymbol [...]` | Splits on "." |
| `AtomList xs` | `List (map compile xs)` | Function calls, data |
| `PropList ps` | `Object (Map.fromList (map compile ps))` | Property objects |

### Dotted Symbol Resolution

The key transformation happens with symbols containing dots:

```haskell
-- In AST
Symbol "obj.field.method"

-- After compilation
DottedSymbol ["obj", "field", "method"]
```

This enables hierarchical property access and namespace resolution.

## AST vs IR

### Design Philosophy

- **AST**: Faithful representation of source syntax
- **IR**: Optimized for evaluation and analysis

### Key Differences

| Aspect | AST | IR |
|--------|-----|----|
| **Purpose** | Syntax representation | Evaluation preparation |
| **Dotted symbols** | `Symbol "x.y.z"` | `DottedSymbol ["x", "y", "z"]` |
| **Complexity** | Simple, direct | Rich, optimized |
| **Evaluation** | Not evaluated directly | Evaluated by interpreter |

### Compilation Pipeline

```
Source Code
    ↓ (parse)
AST
    ↓ (compile)
IR
    ↓ (eval)
Result
```

## AST Construction

### Parser Output

The parser (`parseReactor`) converts source text to AST:

```haskell
parseReactor :: Text -> Either ParseError AST
```

### Example Transformations

#### Simple Expression
```reactor
(+ 1 2)
```
```haskell
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
```haskell
PropList [
    ("name", String "Alice"),
    ("age", Number 30)
]
```

#### Nested Expression
```reactor
(if (> x 0) (* x 2) 0)
```
```haskell
AtomList [
    Symbol "if",
    AtomList [Symbol ">", Symbol "x", Number 0],
    AtomList [Symbol "*", Symbol "x", Number 2],
    Number 0
]
```

## AST Invariants

### Well-Formed AST Properties

1. **Symbol Names**: Valid identifier characters only
2. **List Structure**: Proper nesting and balancing
3. **Property Keys**: Text keys in PropList
4. **Numeric Values**: Valid Scientific numbers

### Error Handling

Invalid AST structures are prevented by:
- Parser validation during parsing
- Type safety in Haskell representation
- Compilation checks during AST→IR conversion

## AST Utilities

### Show Instance
```haskell
instance Show AST where
    show (String s) = "\"" <> T.unpack s <> "\""
    show (Number n) = show n
    show (Symbol s) = T.unpack s
    show (AtomList xs) = "(" <> unwords (map show xs) <> ")"
    show (PropList ps) = "(" <> unwords (map showProp ps) <> ")"
```

### Equality Instance
```haskell
instance Eq AST where
    (String a) == (String b) = a == b
    (Number a) == (Number b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (AtomList a) == (AtomList b) = a == b
    (PropList a) == (PropList b) = a == b
    _ == _ = False
```

## Debugging AST

### Pretty Printing

Use `show` to convert AST back to readable form:

```haskell
let ast = AtomList [Symbol "+", Number 1, Number 2]
print ast  -- Output: (+ 1 2)
```

### AST Inspection

For debugging, you can pattern match on AST nodes:

```haskell
inspectAST :: AST -> String
inspectAST (String s) = "String: " <> show s
inspectAST (Number n) = "Number: " <> show n
inspectAST (Symbol s) = "Symbol: " <> show s
inspectAST (AtomList xs) = "List with " <> show (length xs) <> " elements"
inspectAST (PropList ps) = "Object with " <> show (length ps) <> " properties"
```

## AST Limitations

### Current Restrictions

1. **No Line/Column Info**: AST nodes don't track source locations
2. **No Comments**: Comments are discarded during parsing
3. **No Macros**: AST is literal representation of source
4. **Fixed Structure**: No extensible AST node types

### Future Extensions

Potential enhancements:
- Source location tracking for better error messages
- Comment preservation for documentation tools
- Macro expansion at AST level
- AST optimization passes

## Summary

The Reactor AST provides a clean, simple representation of source code syntax that serves as the foundation for compilation to IR. Its design emphasizes:

- **Direct correspondence** to source syntax
- **Type safety** through Haskell's type system
- **Simplicity** for parsing and transformation
- **Extensibility** for future language features

The AST is the bridge between human-readable source code and machine-evaluable IR, making Reactor's compilation pipeline both elegant and efficient.
