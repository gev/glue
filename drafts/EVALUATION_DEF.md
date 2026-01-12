# Evaluation: Def Special Form

## Overview

`def` is Glue's variable definition special form. It binds a symbol to an evaluated value in the current environment frame.

## Syntax

```
(def symbol expression)
```

## Evaluation Rules

1. **Evaluate expression** - The value to be bound is fully evaluated
2. **Create binding** - Bind the result to `symbol` in current environment frame
3. **Return value** - Return the bound value as the result

## Examples

### Simple Definition
```closure
(def x 42)
```
**Process:**
1. Evaluate `42` → `42`
2. Bind `x` to `42` in current environment
3. Return `42`

### Computed Definition
```closure
(def result (+ 1 2 3))
```
**Process:**
1. Evaluate `(+ 1 2 3)` → `6`
2. Bind `result` to `6` in current environment
3. Return `6`

### Object Definition
```closure
(def user (:name "Alice" :age 30))
```
**Process:**
1. Evaluate `(:name "Alice" :age 30)` → `Object {name: "Alice", age: 30}`
2. Bind `user` to the object in current environment
3. Return the object

## Variable Shadowing

`def` can shadow existing bindings from outer scopes:

```closure
(def x 1)        ; global x = 1
(lambda ()
  (def x 2)      ; local x = 2 (shadows global)
  x)             ; returns 2
x                 ; returns 1 (global still exists)
```

## Scope

Definitions are scoped to the current environment frame:
- **Function scope** - Variables defined in functions are local
- **Global scope** - Top-level definitions are global
- **Block scope** - Future block constructs may create nested scopes

## Error Conditions

### Invalid Symbol
**Cause:** First argument is not a valid symbol
**Example:** `(def 123 "invalid")`
**Error:** Symbol expected

### Evaluation Error
**Cause:** Expression evaluation fails
**Example:** `(def x (/ 1 0))`
**Error:** Division by zero

## Function Definition Sugar

Glue supports Scheme-style function definition syntax:

```closure
(def (function-name param1 param2 ...) body...)
```

This expands to:

```closure
(def function-name (lambda (param1 param2 ...) body...))
```

### Examples

```closure
;; Simple function
(def (square x) (* x x))
;; Expands to: (def square (lambda (x) (* x x)))

;; Multiple parameters
(def (add x y) (+ x y))
;; Expands to: (def add (lambda (x y) (+ x y)))

;; Multiple body expressions (implicit sequence)
(def (factorial n)
  (println "Computing factorial of" n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
;; Expands to: (def factorial (lambda (n) (println ...) (if ...)))
```

### Evaluation

The sugar is transformed at evaluation time:
1. Parse function signature: `(fname param...)`
2. Construct lambda: `(lambda (param...) body...)`
3. Evaluate lambda to create closure
4. Bind closure to `fname`

## Return Value

- **Variable definitions**: Return `Void`
- **Function definitions**: Return the created closure

```closure
;; Variable definition returns Void
(def x 42)  ; returns ()

;; Function definition returns the closure
(def (f x) (* x x))  ; returns <closure>

;; Can use the returned closure immediately
((def (square x) (* x x)) 5)  ; returns 25
```

This allows for more flexible usage patterns while maintaining backward compatibility for variable definitions.

## Implementation Notes

### Environment Modification
`def` modifies the current environment frame by adding a new binding.

### Immutability
While `def` creates new bindings, existing bindings in the same frame cannot be redefined.

### Performance
Environment frames use efficient data structures for fast lookups and updates.
