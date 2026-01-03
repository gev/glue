# Reactor Environment

## Overview

The Reactor environment manages variable bindings and scoping during program execution. Environments are implemented as a stack of frames, where each frame contains a mapping of symbols to values.

## Environment Structure

### Stack of Frames

Environments are organized as a stack of frames:

```haskell
type Environment = [Frame]        -- Stack of frames, searched top to bottom
type Frame = Map Text IR          -- Single frame with symbol-to-IR mappings
```

### Frame Stack

Reactor uses a simple stack of frames:

- **Top Frame**: Most recently pushed frame (current scope)
- **Bottom Frame**: First pushed frame (contains initial bindings)
- **Middle Frames**: Any frames between top and bottom

### Lookup Process

Variable lookup searches from top frame to bottom frame:

```
[Top Frame]     ← Searched first (most local)
[Middle Frame]
[Bottom Frame]  ← Searched last (most global)
```

## Variable Binding Operations

### Definition (`def`)

Creates new bindings in the current (top) frame:

```closure
(def x 42)        ;; Creates x = 42 in current frame
(def y "hello")   ;; Creates y = "hello" in current frame
```

### Assignment (`set`)

Updates existing bindings by searching through the frame stack:

```closure
(set x 100)       ;; Updates x in whichever frame it exists
```

## Scoping Rules

### Lexical Scoping

Variables are resolved in the environment where they are defined:

```closure
(def x 1)               ;; Bottom frame

(lambda ()
  (def y 2)            ;; New top frame
  (+ x y)              ;; → 3
)
```

### Shadowing

Inner scopes can shadow outer scope variables:

```closure
(def x 1)               ;; Global x = 1

(lambda ()
  (def x 2)            ;; Local x = 2 (shadows global)
  x                    ;; → 2 (local)
)

x                       ;; → 1 (global still exists)
```

## Frame Management

### Function Calls

Each function call creates a new frame:

```closure
(lambda (param)
  (def local-var 42)
  (+ param local-var))
```

Execution creates frames:
```
[New Frame: local-var=42, param=value]  ;; Top frame
[Previous Frame]                        ;; Middle frame(s)
[Bottom Frame]                          ;; Original frame
```

### Frame Lifecycle

- **Push**: New frames added when entering functions/blocks
- **Pop**: Frames removed when exiting functions/blocks
- **Persistence**: Bottom frame persists throughout program execution

## Symbol Resolution

### Simple Symbols

Direct lookup in frame stack:

```closure
x  ;; Lookup 'x' from top frame downward
```

### Dotted Symbols

Hierarchical property access:

```closure
obj.field        ;; Access 'field' property of 'obj'
obj.nested.value ;; Deep property access
```

## Error Handling

- **Unbound Variable**: Symbol not found in any frame
- **Scope Violations**: Attempting to access variables outside their scope

## Performance Characteristics

- **Lookup Time**: O(depth) where depth is stack height
- **Frame Operations**: O(1) for push/pop
- **Memory Usage**: Proportional to number of bindings

## See Also

- [Evaluation Documentation](EVALUATION.md) - How environments are used during evaluation
- [Binding Semantics](BINDING_SEMANTICS.md) - Detailed variable binding rules
