# Quote Special Form

## Overview

The `quote` special form returns its argument unevaluated, preventing evaluation of its argument.

## Syntax

```clojure
(quote expr)
'expr
```

## Description

The `quote` special form prevents evaluation of its argument. Instead of evaluating the expression, it returns the expression exactly as written.

This is useful for:
- Creating data structures without evaluating them as function calls
- Working with symbols as values
- Creating lists that should not be evaluated

### Alias

The single quote character `'` is syntactic sugar for `quote`. Both forms are equivalent:

```clojure
(quote x)  ; → 'x
```

## Examples

### Basic Usage

```clojure
;; Returns the symbol 'x unevaluated
(quote x)  ; → x
'x         ; → x

;; Returns the list unevaluated (not evaluated as function call)
(quote (+ 1 2))  ; → (+ 1 2)
'(+ 1 2)         ; → (+ 1 2)

;; Returns the string unevaluated
(quote "hello")  ; → "hello"
'"hello"          ; → "hello"
```

### With Data Structures

```clojure
;; Literal list (not a function call)
'(1 2 3)  ; → (1 2 3)

;; Literal object
'(:name "Alice" :age 30)  ; → (:name "Alice" :age 30)
```

## Related Documents

- [Lambda Special Form](lambda.md) - Creating closures
- [Def Special Form](def.md) - Variable definition
- [Syntax Specification](../syntax.md) - Complete language syntax
