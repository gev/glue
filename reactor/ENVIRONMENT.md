# Reactor Environment

## Overview

The Reactor environment manages variable bindings and scoping during program execution. Environments are implemented as a stack of frames, where each frame contains a mapping of symbols to values.

## Environment Structure

### Stack of Frames

Environments are organized as a stack of frames, where each frame contains a mapping of symbols to values.

### Frame Types

- **Global Frame**: Contains builtin functions, constants, and user-defined globals
- **Function Frames**: Contain function parameters and local variables
- **Block Frames**: Contain variables defined within code blocks

### Lookup Process

Variable lookup searches frames from top to bottom:

```
[Local Frame]     ← Searched first
[Function Frame]
[Global Frame]    ← Searched last
```

## Variable Binding Operations

### Definition (`def`)

Creates new bindings in the current (top) frame:

```reactor
(def x 42)        ;; Creates x = 42 in current frame
(def y "hello")   ;; Creates y = "hello" in current frame
```

### Assignment (`set`)

Updates existing bindings by searching through the frame stack:

```reactor
(set x 100)       ;; Updates x in whichever frame it exists
```

## Scoping Rules

### Lexical Scoping

Variables are resolved in the environment where they are defined:

```reactor
(def global 1)           ;; Global frame

(lambda ()
  (def local 2)         ;; Function frame
  (+ global local)      ;; → 3
)
```

### Shadowing

Inner scopes can shadow outer scope variables:

```reactor
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

```reactor
(lambda (param)
  (def local-var 42)
  (+ param local-var))
```

Execution creates frames:
```
[Function Frame: local-var=42, param=value]
[Caller Frame]
[Global Frame]
```

### Frame Lifecycle

- **Push**: New frames added when entering functions/blocks
- **Pop**: Frames removed when exiting functions/blocks
- **Persistence**: Global frame persists throughout program execution

## Symbol Resolution

### Simple Symbols

Direct lookup in frame stack:

```reactor
x  ;; Lookup 'x' from top frame downward
```

### Dotted Symbols

Hierarchical property access:

```reactor
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
