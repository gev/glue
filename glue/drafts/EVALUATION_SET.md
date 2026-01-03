# Evaluation: Set Special Form

## Overview

`set` updates existing variable bindings or object properties with new values. It is Glue's mutation special form.

## Syntax

```
(set target expression)
```

## Evaluation Rules

1. **Evaluate expression** - The new value is fully evaluated
2. **Update target** - Modify the target binding or property
3. **Return value** - Return the assigned value

## Target Types

### Variable Update

**Syntax:** `(set symbol expression)`

**Process:**
1. Evaluate `expression` to get new value
2. Find existing binding for `symbol` in environment
3. Update binding with new value
4. Return the new value

**Examples:**
```closure
(def x 1)
(set x 42)  ; → 42, x is now 42
```

### Property Update

**Syntax:** `(set object.property expression)`

**Process:**
1. Evaluate `expression` to get new value
2. Resolve `object` to existing object value
3. Update or add property with new value
4. Return the new value

**Examples:**
```closure
(def user (:name "Alice" :age 25))
(set user.age 26)     ; → 26, user.age is now 26
(set user.email "alice@example.com")  ; → "alice@example.com"
```

## Nested Property Access

### Dotted Property Paths
```closure
(def config (:database (:host "localhost" :port 5432)))
(set config.database.port 3306)  ; Update nested property
```

### Module Property Updates
```closure
(set math.constants.pi 3.14)  ; Update module export
```

## Evaluation Order

Arguments are evaluated in order:
1. Target expression (for property access)
2. Value expression
3. Target update

## Error Conditions

### Unbound Variable
**Cause:** Variable name doesn't exist in environment
**Example:** `(set nonexistent 42)`
**Error:** UnboundVariable

### Non-Object Property Access
**Cause:** Attempting property access on non-object value
**Example:** `(set "string".property 42)`
**Error:** NotAnObject

### Property Not Found
**Cause:** Object doesn't have the specified property
**Example:** `(set obj.missing 42)` where obj doesn't have 'missing'
**Error:** PropertyNotFound (may create new property instead)

## Return Value

`set` always returns the assigned value:

```closure
(def result (set x 100))  ; result = 100
```

## Scope and Shadowing

### Environment Frames
`set` searches for variables starting from the current frame outward:

```closure
(def x 1)              ; global x
(lambda ()
  (def x 2)            ; local x shadows global
  (set x 3)            ; updates local x
  x)                   ; returns 3
x                       ; returns 1 (global unchanged)
```

### Closure Capture
`set` can modify variables captured by closures:

```closure
(def counter
  (let ((count 0))
    (lambda () (set count (+ count 1)) count)))

(counter)  ; → 1
(counter)  ; → 2
```

## Implementation Notes

### Environment Modification
`set` modifies existing environment frames rather than creating new ones.

### Property Semantics
Object property updates may create new properties if they don't exist, depending on implementation.

### Performance
Variable updates require environment traversal, property updates require object access.

### Immutability
While `set` provides mutation, Glue maintains functional programming principles through explicit mutation operations.
