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

## Parsing Rules

Reactor's parser follows these rules to convert source text into AST:

### Expression Priority

Expressions are parsed in this order of priority (highest first):

1. **Quoted expressions** (`'expr`)
2. **Lists and property objects** (`(expr...)`, `(:key val...)`)
3. **String literals** (`"text"`)
4. **Numbers** (`42`, `3.14`)
5. **Symbols** (`x`, `+`, `obj.field`)

### Whitespace & Comments

- **Whitespace**: Spaces, tabs, newlines are ignored (except in strings)
- **Line comments**: `;` followed by any text until end of line
- **Block comments**: `#|` followed by any text until `|#`

### Numbers

**Rule:** Parse numeric literals using scientific notation.

**Valid patterns:**
- Integers: `42`, `-15`, `0`
- Decimals: `3.14159`, `-273.15`
- Scientific: `1.23e-4`, `6.02e23`

**Invalid patterns:**
- Multiple dots: `1.2.3`
- Hex/binary: `0xFF`, `0b1010`
- Leading zeros: `007` (parsed as `7`)

### Strings

**Rule:** Parse text between double quotes with escape sequences.

**Valid patterns:**
- Simple: `"hello"`
- With escapes: `"with \"quotes\" and \n newlines"`
- Empty: `""`

**Invalid patterns:**
- Unclosed: `"missing end`
- Invalid escapes: `"bad \x escape"`

### Symbols

**Rule:** Parse identifiers starting with letter, containing letters, digits, and special chars.

**Valid characters:** letters, digits, `-`, `.`, `_`, `:`, `!`, `?`, `\`, `=`, `>`, `<`, `/`, `*`, `+`, `%`

**Valid patterns:**
- Variables: `x`, `my-var`, `result_1`
- Operators: `+`, `-`, `*`, `/`, `=`, `>`, `<`
- Dotted paths: `math.pi`, `obj.field.method`

**Invalid patterns:**
- Starting with digit: `123abc`
- Containing spaces: `my var`
- Empty symbols

### Quoted Expressions

**Rule:** `'` followed by any expression becomes `(quote expression)`.

**Examples:**
- `'x` → `(quote x)`
- `'(+ 1 2)` → `(quote (+ 1 2))`
- `''foo` → `(quote (quote foo))`

### Lists

**Rule:** Parenthesized expressions create lists.

**Syntax:** `(expr1 expr2 expr3 ...)`

**Special cases:**
- Empty list: `()` → `AtomList []`
- Single element: `(x)` → `AtomList [x]`
- Nested: `((+ 1 2))` → `AtomList [AtomList [Symbol "+", Number 1, Number 2]]`

### Property Objects

**Rule:** Lists starting with `:`-prefixed symbols create property objects.

**Syntax:** `(:key1 value1 :key2 value2 ...)`

**Validation:**
- Keys must start with `:`
- Each key must have a corresponding value
- Cannot mix properties with regular arguments

**Examples:**
- Valid: `(:name "Alice" :age 30)`
- Invalid: `(:name)` (missing value)
- Invalid: `(:name "Alice" arg)` (mixed content)

### Parsing Pipeline

```
Source Text
    ↓ Tokenize (split into meaningful units)
Tokens
    ↓ Parse grammar (apply parsing rules)
Raw AST
    ↓ Validate structure (check property objects, etc.)
Validated AST
```

### Error Prevention

The parser prevents invalid AST through:

1. **Grammar rules** - Only valid syntax parses
2. **Type safety** - Haskell types prevent invalid combinations
3. **Validation** - Property objects checked for correctness
4. **Lexical rules** - Invalid characters rejected

## Syntax Errors

### Error Types

#### `MixedContent`
```haskell
data ParserError = MixedContent Text
```

**Trigger:** Mixing properties and positional arguments
```reactor
;; Invalid
(+ :x 1 2)  ;; ERROR: Property ':x' mixed with positional args

;; Valid alternatives
(+ 1 2)           ;; All positional
(:x 1 :y 2)       ;; All properties
```

#### `UnpairedProperty`
```haskell
data ParserError = UnpairedProperty Text
```

**Trigger:** Property without value
```reactor
;; Invalid
(:name "Alice" :age)  ;; ERROR: ':age' has no value

;; Valid
(:name "Alice" :age 30)
```

#### `ReservedKeyword`
```haskell
data ParserError = ReservedKeyword Text
```

**Trigger:** Using reserved identifiers
```reactor
;; Invalid (if 'def' is reserved)
(def x 1)  ;; ERROR: 'def' is reserved
```

#### `SyntaxError`
```haskell
data ParserError = SyntaxError Text
```

**Trigger:** General parsing failures
- Unmatched parentheses
- Invalid characters
- Malformed numbers/strings
- Unexpected tokens

### Error Examples

#### Unmatched Parentheses
```reactor
;; Input: (+ 1 2
;; Error: SyntaxError "unexpected end of input"
```

#### Invalid Number
```reactor
;; Input: 1.2.3
;; Error: SyntaxError "unexpected '.'"
```

#### Mixed Content
```reactor
;; Input: (f arg1 :key val)
;; Error: MixedContent ":key"
```

#### Unpaired Property
```reactor
;; Input: (:name "Alice" :age)
;; Error: UnpairedProperty ":age"
```

### Error Recovery

The parser uses Megaparsec's error reporting for detailed diagnostics:

```haskell
parserError :: ParseErrorBundle Text ParserError -> ParserError
parserError bundle =
    case head (bundleErrors bundle) of
        FancyError _ (Set.toList -> [ErrorCustom e]) -> e
        _ -> SyntaxError (T.pack $ errorBundlePretty bundle)
```

This provides both custom Reactor errors and fallback to Megaparsec's detailed error messages.

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
