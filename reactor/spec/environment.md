# Environment Specification

## Overview

The Reactor environment manages variable bindings and scoping during program execution. Environments provide the runtime context for symbol resolution, enabling lexical scoping and proper variable capture in closures. The environment system supports both simple variable bindings and complex module-based scoping hierarchies.

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

- **Initial Environment**: Contains subset of builtin and custom functions and initial global bindings
- **Function Calls**: New frames pushed for function parameters and local variables
- **Block Scopes**: New frames for lexical blocks with local bindings
- **Module Evaluation**: Isolated environments for module execution

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

## Module Environment Integration

### Dual-Registry Module System

Reactor uses separate registries for module management:

- **Module Registry**: Stores module metadata and unevaluated bodies
- **Imported Module Cache**: Stores evaluated module results for reuse

### Module Evaluation Isolation

Modules are evaluated in isolated environments:

- **Root Environment**: Original environment with builtins preserved
- **Evaluation Environment**: Isolated stack for module execution
- **Export Merging**: Exported bindings integrated into modules registry

### Lazy Module Loading

Modules are evaluated once and cached:

- **First Import**: Evaluate module body, cache results
- **Subsequent Imports**: Return cached results directly
- **Global Sharing**: One evaluation serves all importers

## Module Import into Local Frames

### Local Scope Import Behavior

**Important**: Module imports are **local to the frame** where the import occurs, not global. Imported modules and their symbols are only available within the scope that performed the import.

```closure
;; Global scope - no imports
(def x 1)

(lambda ()
  (import math)      ;; Import only available in this lambda
  (+ pi x))          ;; 'pi' accessible here

;; Outside lambda - 'pi' not available
;; (This would cause UnboundVariable error)
;; (+ pi x)
```

### Import Process Overview

When a module is imported, Reactor integrates its exported symbols into the **current local environment frame** through a **dual-access mechanism**:

1. **Direct Symbol Access**: Exported symbols become direct variables in the importing scope
2. **Module Object Access**: The module name provides access to the complete module namespace

### Direct Symbol Integration

Imported symbols are merged directly into the current environment frame:

```closure
(import math)
(+ pi 1)        ;; 'pi' is now accessible as a direct variable
```

**Implementation Details:**
- Exported symbols become top-level bindings in the importing frame
- No namespace prefix required for commonly used symbols
- Enables clean, flat access to module functionality

### Module Object for Hierarchical Access

The complete module is stored as a **Module object** under its name:

```closure
(import math.const)
math.const.pi    ;; Access through module namespace
```

### Dotted Symbol Resolution

The environment supports **hierarchical symbol resolution** for dotted access:

```closure
object.property.field      ;; Nested object access
module.submodule.symbol    ;; Deep module access
;; Mixed deep access
module.submodule.symbol.object.property.field
```

**Environment Lookup:**
For `DottedSymbol ["base", "prop1", "prop2", ...]`:

1. **Always** search `"base"` in the environment first
2. If found, delegate property access navigation to evaluation
3. See [Evaluation](evaluation/) for complete dotted symbol resolution algorithm

### Import Isolation and Security

Modules are evaluated in **completely isolated environments**:

- **Evaluation Context**: Only builtin or custom functions available during module loading
- **No Import Scope Access**: Modules cannot reference variables from importing code
- **Clean Separation**: Prevents unintended coupling between modules and importers

### Environment Frame Structure After Import

```closure
Before Import:
[user_vars, builtins]

After Import:
[imported_symbols, module_objects, user_vars, builtins]
```

Imported modules create new frames containing both direct symbol access and module object references, maintaining clean separation while providing flexible access patterns.

## Symbol Resolution

### Simple Symbols

Direct lookup in frame stack:

```closure
x        ;; Lookup 'x' from top frame downward
```

### Dotted Symbols

Hierarchical property access through object/module chains:

```closure
obj.field        ;; Access 'field' property of 'obj'
module.func      ;; Access 'func' from module namespace
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
- **Cache Benefits**: Module imports provide O(1) cached lookups after first evaluation
- **Builtin Access**: Shared builtin frames reduce average lookup time

### Memory Usage

- **Frame Storage**: Proportional to number of bindings per scope
- **Stack Depth**: Limited by language implementation constraints
- **Caching**: Module results cached globally to avoid duplication

### Frame Operations

- **Push/Pop**: O(1) frame stack operations
- **Definition**: O(1) local frame updates
- **Lookup/Assignment**: O(depth) for variable location search

## Relationship to Other Components

### IR (Intermediate Representation)

- Environments enable [IR](ir.md) closure capture and lexical scoping
- [IR](ir.md) values stored in environment frames
