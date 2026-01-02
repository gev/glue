# Environment Specification

## Overview

The Reactor environment manages variable bindings and scoping during program execution. Environments provide the runtime context for symbol resolution, enabling lexical scoping and proper variable capture in closures. The environment system supports both simple variable bindings and complex module-based scoping hierarchies.

## Environment Structure

### Stack of Frames

Environments are organized as a stack of frames, searched from top to bottom:

```
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

```reactor
(def x 1)               ;; Global scope

(lambda ()
  (def y 2)            ;; Function scope
  (+ x y))             ;; Resolves x from global, y from local
```

### Shadowing

Inner scopes can shadow variables from outer scopes:

```reactor
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
- **Export Merging**: Exported bindings integrated into importing scope

### Lazy Module Loading

Modules are evaluated once and cached:

- **First Import**: Evaluate module body, cache results
- **Subsequent Imports**: Return cached results directly
- **Global Sharing**: One evaluation serves all importers

## Module Import into Local Frames

### Import Process Overview

When a module is imported, Reactor integrates its exported symbols into the local environment through a **dual-access mechanism**:

1. **Direct Symbol Access**: Exported symbols become direct variables in the importing scope
2. **Module Object Access**: The module name provides access to the complete module namespace

### Direct Symbol Integration

Imported symbols are merged directly into the current environment frame:

```reactor
(import math)
(+ pi 1)        ;; 'pi' is now accessible as a direct variable
```

**Implementation Details:**
- Exported symbols become top-level bindings in the importing frame
- No namespace prefix required for commonly used symbols
- Enables clean, flat access to module functionality

### Module Object for Hierarchical Access

The complete module is stored as a **Module object** under its name:

```reactor
(import math)
math.constants.pi    ;; Access through module namespace
math.trig.sin        ;; Hierarchical dotted access
```

**Implementation Details:**
- Module name becomes a variable containing `Module (Map Text IR)`
- Enables `module.property` and `module.submodule.property` syntax
- Preserves full module structure for complex APIs

### Dotted Symbol Resolution

The environment supports **hierarchical symbol resolution** for dotted access:

```reactor
module.submodule.symbol    ;; Deep module access
object.property.field      ;; Nested object access
```

**Resolution Algorithm:**
1. Try longest prefix as complete symbol name
2. Fall back to shorter prefixes with property access
3. Support both module and object hierarchies

### Import Isolation and Security

Modules are evaluated in **completely isolated environments**:

- **Evaluation Context**: Only builtin functions available during module loading
- **No Import Scope Access**: Modules cannot reference variables from importing code
- **Clean Separation**: Prevents unintended coupling between modules and importers

### Caching and Performance

- **Lazy Evaluation**: Modules evaluated only when first imported
- **Global Caching**: One evaluation serves all importing contexts
- **Memory Efficiency**: Shared builtin frames across all module evaluations
- **Fast Subsequent Access**: Cached results provide O(1) import performance

### Environment Frame Structure After Import

```
Before Import:
[user_vars, builtins]

After Import:
[imported_symbols, module_objects, user_vars, builtins]
```

Imported modules create new frames containing both direct symbol access and module object references, maintaining clean separation while providing flexible access patterns.

## Symbol Resolution

### Simple Symbols

Direct lookup in frame stack:

```reactor
x        ;; Lookup 'x' from top frame downward
```

### Dotted Symbols

Hierarchical property access through object/module chains:

```reactor
obj.field        ;; Access 'field' property of 'obj'
module.func      ;; Access 'func' from module namespace
```

## Error Conditions

### Unbound Variables

Attempting to reference undefined symbols:

- **Lookup Failure**: Symbol not found in any frame
- **Scope Violation**: Accessing variables outside their defined scope

### Assignment Errors

Attempting to assign to undefined variables:

- **Cannot Set Unbound**: Assignment to non-existent variables
- **Scope Restrictions**: Assignment cannot create new bindings

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
- **Assignment**: O(depth) for variable location search

## Relationship to Other Components

### IR (Intermediate Representation)

- Environments enable [IR](ir.md) closure capture and lexical scoping
- [IR](ir.md) values stored in environment frames
- Environment threading supports [IR](ir.md) evaluation semantics

### Evaluation

- Evaluation traverses [IR](ir.md) structures using environment context
- Environment provides symbol resolution during evaluation
- Frame management coordinated with evaluation state

### Modules

- Module system extends environment with namespace hierarchies
- See [Module System](module-system.md) for namespace organization
- Environment isolation ensures clean module boundaries

## Implementation Notes

### Environment Threading

Closures capture complete environment snapshots:

- **Capture**: Environment copied at closure creation time
- **Persistence**: Captured environment survives original scope exit
- **Isolation**: Closure environments independent of current execution context

### Root Environment Preservation

The original environment passed to program execution is preserved:

- **Builtin Access**: Consistent access to builtin functions
- **Module Loading**: Reliable context for module evaluation
- **Security**: Prevents environment pollution during execution

### Future Extensions

- **Namespace Frames**: Qualified imports creating named environment frames
- **Hot Reloading**: Environment snapshots for dynamic module updates
- **Environment Serialization**: Persistence and restoration of execution contexts
