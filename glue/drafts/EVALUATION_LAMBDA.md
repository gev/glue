# Evaluation: Lambda Special Form

## Overview

`lambda` creates anonymous functions (closures) that capture their definition environment. It is Reactor's function definition special form.

## Syntax

```
(lambda (parameter1 parameter2 ...) body-expression)
```

## Evaluation Rules

1. **Do not evaluate parameters** - Parameter names are binding symbols
2. **Do not evaluate body** - Body expression is stored for later execution
3. **Capture environment** - Current environment is captured for closure
4. **Create closure** - Return closure object with parameters, body, and environment

## Examples

### Simple Lambda
```reactor
(lambda (x) (* x x))
```
**Process:**
1. Parameters: `[x]`
2. Body: `(* x x)`
3. Environment: Current scope captured
4. Result: Closure object

### Multiple Parameters
```reactor
(lambda (a b) (+ a b))
```
**Process:**
1. Parameters: `[a, b]`
2. Body: `(+ a b)`
3. Environment: Current scope captured
4. Result: Closure object

### No Parameters
```reactor
(lambda () "hello")
```
**Process:**
1. Parameters: `[]`
2. Body: `"hello"`
3. Environment: Current scope captured
4. Result: Closure object

## Closure Behavior

### Lexical Scoping
Closures capture variables from their definition scope:

```reactor
(def multiplier 2)
(def make-multiplier
  (lambda (x) (lambda (y) (* x y multiplier))))

(def double (make-multiplier 2))
(def triple (make-multiplier 3))

(double 5)  ; → 10 (2 * 5 * 2)
(triple 5)  ; → 30 (3 * 5 * 2)
```

### Environment Capture
The captured environment includes all accessible variables at definition time.

## Function Application

Lambda-created closures are applied using normal function call syntax:

```reactor
((lambda (x) (* x 2)) 5)  ; → 10
```

## Parameter Binding

### Positional Parameters
Parameters are bound positionally during application:

```reactor
(def add (lambda (a b) (+ a b)))
(add 3 4)  ; a=3, b=4 → 7
```

### Scope
Parameters are bound in a new environment frame for function execution.

## Error Conditions

### Parameter Validation
**Cause:** Duplicate parameter names
**Example:** `(lambda (x x) (+ x 1))`
**Error:** Duplicate parameter names

### Non-Symbol Parameters
**Cause:** Parameter list contains non-symbols
**Example:** `(lambda (x 123) x)`
**Error:** Parameters must be symbols

## Implementation Notes

### Closure Representation
Closures store:
- Parameter names (symbols)
- Body expression (IR node)
- Captured environment (at definition time)

### First-Class Functions
Closures are first-class values that can be:
- Stored in variables
- Passed as arguments
- Returned from functions
- Stored in data structures

### Performance
Environment capture is shallow - only references are stored, not deep copies.

### Memory Management
Captured environments may retain references to large data structures. Garbage collection handles cleanup when closures go out of scope.
