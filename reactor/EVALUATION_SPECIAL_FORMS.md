# Evaluation: Special Forms

## Overview

Special forms are built-in operations that don't follow normal evaluation rules. They control evaluation itself, perform side effects, or create new evaluation contexts. Reactor's special forms include `def`, `lambda`, `quote`, and `set`.

## Def (Definition)

**Input:** `(def symbol value)`
**Process:** Bind symbol to evaluated value in current environment
**Output:** The bound value

### Evaluation Rules
1. Evaluate the `value` expression
2. Bind result to `symbol` in current environment frame
3. Return the bound value

### Examples
- `(def x 42)` → binds 42 to x, returns 42
- `(def result (+ 1 2))` → evaluates (+ 1 2) to 3, binds to result, returns 3

### Variable Shadowing
New definitions can shadow existing bindings in outer scopes.

## Lambda (Function Creation)

**Input:** `(lambda (param1 param2 ...) body)`
**Process:** Create closure with parameters and body
**Output:** Closure object capturing current environment

### Evaluation Rules
1. Do not evaluate parameters (they are binding names)
2. Do not evaluate body (it will be evaluated when called)
3. Create closure with parameter names, body expression, and current environment
4. Return the closure object

### Examples
- `(lambda (x) (* x x))` → closure that squares its argument
- `(lambda (a b) (+ a b))` → closure that adds two arguments

### Lexical Scoping
Closures capture their definition environment, enabling proper lexical scoping.

## Quote (Data Literal)

**Input:** `(quote expression)`
**Process:** Return expression as data without evaluation
**Output:** The unevaluated expression as an IR value

### Evaluation Rules
1. Do not evaluate the `expression`
2. Convert AST representation to IR data structure
3. Return as literal data

### Examples
- `(quote (+ 1 2))` → returns `List [Symbol "+", Number 1, Number 2]`
- `(quote (list a b c))` → returns unevaluated list structure

### Shorthand
The `'` syntax is shorthand for quote:
- `'(+ 1 2)` ≡ `(quote (+ 1 2))`

## Set (Mutation)

**Input:** `(set target value)`
**Process:** Update existing binding with new value
**Output:** The new value

### Target Types

#### Variable Update
- **Input:** `(set symbol value)`
- **Process:** Find and update existing variable binding
- **Rules:**
  1. Evaluate the `value` expression
  2. Search environment for existing `symbol` binding
  3. Update binding with new value
  4. Return the new value

#### Property Update
- **Input:** `(set object.property value)`
- **Process:** Update property on existing object
- **Rules:**
  1. Evaluate the `value` expression
  2. Resolve `object` to existing object
  3. Update or add property with new value
  4. Return the new value

### Examples
- `(set x 100)` → updates variable x to 100
- `(set user.name "Bob")` → updates user object's name property
- `(set config.debug true)` → updates nested property

## Special Form Characteristics

### Non-Standard Evaluation
Special forms don't evaluate all arguments:
- `def`: evaluates value, not symbol
- `lambda`: evaluates nothing (creates binding context)
- `quote`: evaluates nothing (returns literal)
- `set`: evaluates value, target depends on form

### Side Effects
Special forms can modify evaluation state:
- `def`: adds bindings to environment
- `set`: modifies existing bindings
- `lambda`: creates closure (no side effect)
- `quote`: pure data transformation

### Return Values
Special forms return meaningful values:
- `def`: returns the defined value
- `lambda`: returns the closure
- `quote`: returns the quoted data
- `set`: returns the assigned value

## Error Conditions

### Def Errors
- Symbol must be valid identifier
- Value evaluation may fail

### Lambda Errors
- Parameter names must be valid symbols
- Duplicate parameter names not allowed

### Quote Errors
- Any expression can be quoted (no errors)

### Set Errors
- Target variable must exist (for variable update)
- Target object must exist (for property update)
- Property access may fail on non-objects

## Implementation Notes

### Environment Modification
Special forms that modify environment (`def`, `set`) affect the current evaluation context and persist for subsequent evaluations.

### Closure Creation
`lambda` creates first-class functions that can be stored, passed as arguments, and called multiple times.

### Data vs Code Distinction
`quote` enables the crucial distinction between code (to be evaluated) and data (literal values) in Lisp-style languages.

### Mutation Semantics
`set` provides controlled mutation while maintaining functional programming principles through explicit operations.
