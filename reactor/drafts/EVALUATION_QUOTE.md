# Evaluation: Quote Special Form

## Overview

`quote` prevents evaluation of its argument, returning the argument as literal data. It is Reactor's mechanism for representing code as data.

## Syntax

```
(quote expression)
```

Shorthand: `'expression`

## Evaluation Rules

1. **Do not evaluate expression** - The argument is returned as-is
2. **Convert to data** - AST representation becomes IR data structure
3. **Return literal** - Result is unevaluated data

## Examples

### Basic Quote
```reactor
(quote (+ 1 2))
```
**Process:**
1. Do not evaluate `(+ 1 2)`
2. Convert to data structure
3. Return: `List [Symbol "+", Number 1, Number 2]`

### Shorthand Syntax
```reactor
'(+ 1 2)  ; Same as (quote (+ 1 2))
```
**Result:** `List [Symbol "+", Number 1, Number 2]`

### Complex Data
```reactor
(quote (:name "Alice" :data (1 2 3)))
```
**Result:** `Object {name: "Alice", data: List [1, 2, 3]}`

## Code vs Data Distinction

### Without Quote (Code)
```reactor
(+ 1 2)  ; → 3 (evaluated)
```

### With Quote (Data)
```reactor
'(+ 1 2)  ; → (+ 1 2) as data
```

## Nested Quotes

### Single Quote
```reactor
'(a b c)  ; → List [Symbol "a", Symbol "b", Symbol "c"]
```

### Double Quote
```reactor
''foo  ; → List [Symbol "quote", Symbol "foo"]
```
**Process:** `''foo` ≡ `'(quote foo)` ≡ `(quote (quote foo))`

## Data Structure Conversion

### Lists
Quoted lists become data lists:
```reactor
'(1 2 3)  ; → List [Number 1, Number 2, Number 3]
```

### Objects
Quoted objects become data objects:
```reactor
'(:x 1 :y 2)  ; → Object {x: 1, y: 2}
```

### Symbols
Quoted symbols remain as symbols:
```reactor
'foo  ; → Symbol "foo"
```

## Use Cases

### Metaprogramming
```reactor
(def code '(+ x y))
(def x 1)
(def y 2)
(eval code)  ; → 3
```

### Data Literals
```reactor
(def config '(:debug true :port 8080))
```

### DSL Construction
```reactor
(def query '(select * from users where active = true))
```

## Error Conditions

Quote has no evaluation errors since it doesn't evaluate its argument. Any expression can be quoted.

## Implementation Notes

### AST to IR Conversion
Quote converts the quoted AST directly to equivalent IR data structures without evaluation.

### Symbol Preservation
Symbols in quoted expressions remain as symbols, not variable lookups.

### Structural Identity
Quoted structures maintain their original structure and nesting.

### Performance
Quote is a zero-cost operation - no computation is performed, only data structure conversion.
