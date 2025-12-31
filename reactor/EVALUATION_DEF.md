# Evaluation: Def Special Form

## Overview

`def` is Reactor's variable definition special form. It binds a symbol to an evaluated value in the current environment frame.

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
```reactor
(def x 42)
```
**Process:**
1. Evaluate `42` → `42`
2. Bind `x` to `42` in current environment
3. Return `42`

### Computed Definition
```reactor
(def result (+ 1 2 3))
```
**Process:**
1. Evaluate `(+ 1 2 3)` → `6`
2. Bind `result` to `6` in current environment
3. Return `6`

### Object Definition
```reactor
(def user (:name "Alice" :age 30))
```
**Process:**
1. Evaluate `(:name "Alice" :age 30)` → `Object {name: "Alice", age: 30}`
2. Bind `user` to the object in current environment
3. Return the object

## Variable Shadowing

`def` can shadow existing bindings from outer scopes:

```reactor
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

## Return Value

`def` always returns the value being bound, enabling chaining:

```reactor
(def config
  (def defaults (:debug false :port 8080))
  (:inherit defaults :timeout 5000))
```

## Implementation Notes

### Environment Modification
`def` modifies the current environment frame by adding a new binding.

### Immutability
While `def` creates new bindings, existing bindings in the same frame cannot be redefined.

### Performance
Environment frames use efficient data structures for fast lookups and updates.
