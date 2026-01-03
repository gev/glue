# Environment Specification

## Overview

The Glue environment manages variable bindings and scoping during program execution. Environments provide the runtime context for symbol resolution, enabling lexical scoping and proper variable capture in closures.

## Environment Structure

### Stack of Frames

Environments are organized as a stack of frames, searched from top to bottom:

```closure
type Env = [Frame]        -- Stack of frames, searched top to bottom
type Frame = Map Text IR  -- Single frame with symbol-to-IR mappings
```

### Frame Stack Organization

- **Top Frame**: Most recently pushed frame (current/local scope)
- **Bottom Frame**: First pushed frame (global scope with initial bindings)
- **Middle Frames**: Intermediate scopes (function calls, block scopes)

### Environment Lifecycle

Environments are created and managed throughout program execution:

- **Initial Environment**: Contains builtin functions and initial global bindings
- **Function Calls**: New frames pushed for function parameters and local variables
- **Block Scopes**: New frames for lexical blocks with local bindings

## Variable Operations

### Variable Lookup

Variable resolution searches the frame stack from top to bottom:

- **Local Lookup**: Search only the top frame (current scope)
- **Full Lookup**: Search entire stack until variable found or stack exhausted

### Variable Definition

Creates new bindings in the current (top) frame:

- **Local Definition**: Adds binding to current frame
- **Shadowing**: Local definitions can shadow variables in outer scopes

### Variable Assignment

Updates existing bindings by searching through the frame stack:

- **Update**: Modifies binding in whichever frame contains the variable
- **Scope Restriction**: Cannot create new bindings via assignment

## Scoping Rules

### Lexical Scoping

Variables are resolved in the environment where they are defined, following lexical scope rules:

```closure
(def x 1)               ;; Global scope

(lambda ()
  (def y 2)            ;; Function scope
  (+ x y))             ;; Resolves x from global, y from local
```

### Shadowing

Inner scopes can shadow variables from outer scopes:

```closure
(def x 1)               ;; Global x = 1

(lambda ()
  (def x 2)            ;; Local x = 2 (shadows global)
  x)                   ;; Returns 2 (local)

x                       ;; Returns 1 (global unchanged)
```

### Frame Management

#### Function Call Frames

Each function call creates a new frame containing:

- **Parameters**: Function arguments bound to parameter names
- **Local Variables**: Variables defined within the function body

#### Block Scope Frames

Lexical blocks create new frames for local bindings:

- **Let Bindings**: Local variables in let expressions
- **Block Expressions**: Variables scoped to block lifetime

## Symbol Resolution

### Simple Symbols

Direct lookup in frame stack:

```closure
x        ;; Lookup 'x' from top frame downward
```

### Dotted Symbols

Hierarchical property access through object chains:

```closure
obj.field        ;; Access 'field' property of 'obj'
user.address.city ;; Deep property access
```

## Error Conditions

### Unbound Variables

Attempting to reference undefined symbols results in `UnboundVariable` error:

- **Symbol not found**: Variable not defined in current or outer scopes (lexical scoping violation)

### Assignment Errors

Attempting to assign to undefined variables results in `CanNotSetUnboundVariable` error:

- **Cannot Set Unbound**: Assignment to non-existent variables not allowed
- **Scope Restrictions**: Assignment cannot create new variable bindings

## Performance Characteristics

### Lookup Performance

- **Time Complexity**: O(depth) where depth is environment stack height
- **Builtin Access**: Shared builtin frames reduce average lookup time

### Memory Usage

- **Frame Storage**: Proportional to number of bindings per scope
- **Stack Depth**: Limited by language implementation constraints

### Frame Operations

- **Push/Pop**: O(1) frame stack operations
- **Definition**: O(1) local frame updates
- **Lookup/Assignment**: O(depth) for variable location search

## Relationship to Other Components

### IR (Intermediate Representation)

- Environments enable [IR](ir.md) closure capture and lexical scoping
- [IR](ir.md) values stored in environment frames
