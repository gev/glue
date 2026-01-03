# Reactor Binding Semantics

## Overview

Reactor provides sophisticated variable binding semantics with clear distinctions between definition, mutation, and scoping. This document explains the semantics of `import`, `def`, and `set` operations with both dotted and non-dotted notation.

## Core Operations

### `def` - Variable Definition (Lexical Scope)

**Purpose**: Creates new bindings in the current lexical environment.

#### Simple Symbols
```reactor
(def x 42)        ;; Creates local variable x = 42
(def y "hello")   ;; Creates local variable y = "hello"
```

#### Dotted Symbols (Object Extension)
```reactor
(def obj.a 1)     ;; Creates local obj = {:a 1}
(def obj.b 2)     ;; Extends local obj = {:a 1, :b 2}
```

**Key Properties**:
- ✅ Creates new lexical bindings
- ✅ Never modifies existing global state
- ✅ For dotted symbols: merges into existing local objects or creates new ones
- ✅ Local scope only (lexical shadowing)

### `set` - Variable Mutation (Global Scope)

**Purpose**: Modifies existing variables in the global environment.

#### Simple Symbols
```reactor
(set x 100)       ;; Updates global variable x to 100
```

#### Dotted Symbols (Property Setting)
```reactor
(set obj.field value)  ;; Updates global obj.field
```

**Key Properties**:
- ✅ Modifies existing global variables
- ✅ Requires target to already exist
- ✅ Global scope (affects all code)
- ✅ For dotted notation: uses `DottedSymbol` AST nodes

### `import` - Module Incorporation (Lexical Scope)

**Purpose**: Brings module exports into the current lexical environment.

#### Simple Modules
```reactor
(import math.x)   ;; Creates local math = (:x module)
```

#### Dotted Modules
```reactor
(import math.x.y) ;; Creates hierarchical local structure math = (:x (:y module))
```

**Key Properties**:
- ✅ Creates lexical bindings to modules
- ✅ Local scope (can be shadowed)
- ✅ Module objects are immutable
- ✅ Hierarchical namespace construction

## Scope Semantics

### Lexical vs Dynamic Scope

```reactor
;; Global scope
(def global-obj (:a 1))

;; Function scope - lexical extension
(lambda ()
  (def global-obj.b 2)    ;; Creates local global-obj = {:a 1, :b 2}
  global-obj              ;; → {:a 1, :b 2} (local)
  ;; Global global-obj unchanged!
)

;; Global mutation
(set global-obj.c 3)      ;; Modifies actual global object
global-obj                ;; → {:a 1, :c 3} (global mutation)
```

### Shadowing Behavior

```reactor
(def x 1)                 ;; Global x = 1

(lambda ()
  (def x 2)              ;; Local x = 2 (shadows global)
  x                      ;; → 2 (local)
)

x                          ;; → 1 (global still exists)
```

## Object Construction Patterns

### Pattern 1: Global Object Building with `set`

```reactor
(def config (:debug false))
(set config.logging.level "info")    ;; Requires config to exist
(set config.database.host "localhost")
;; Result: config = {:debug false, :logging {:level "info"}, :database {:host "localhost"}}
```

### Pattern 2: Local Object Extension with `def`

```reactor
(lambda (base-config)
  (def base-config.timeout 30)       ;; Local extension
  (def base-config.retries 3)        ;; More local extensions
  (use-extended-config base-config)  ;; Local config with additions
  ;; base-config unchanged globally
)
```

### Pattern 3: Module Composition with `import`

```reactor
(import math.x)        ;; Local binding: x = math.x
(import math.y)        ;; Local binding: y = math.y
(x.cos 0.5)           ;; Use local x
(y.log 1000)          ;; Use local y
```

## Duality Between Operations

### Object vs Module Duality

| Aspect | Objects (`def`/`set`) | Modules (`import`) |
|--------|----------------------|-------------------|
| **Creation** | `(def x (:a 1))` | `(import math.x)` |
| **Extension** | `(def x.b 2)` | `(import math.y)` |
| **Access** | `x.a`, `x.b` | `x.cos`, `y.log` |
| **Scope** | Lexical/Global | Lexical |
| **Mutability** | Mutable | Immutable |

### Data vs Code Duality

- **`def`**: Operates on **data objects** (mutable structures)
- **`import`**: Operates on **code modules** (immutable functions)
- **Both**: Create hierarchical structures through dotted notation
- **Both**: Work in lexical scope
- **Result**: Unified composition system for data and code

## Error Conditions

### `def` Errors
```reactor
(def x.y 1)      ;; OK - creates local object
(def nonexistent.field 2)  ;; OK - creates new local object
;; No errors - always succeeds by creating local bindings
```

### `set` Errors
```reactor
(set nonexistent 1)        ;; ERROR: unbound variable
(set nonexistent.field 1)  ;; ERROR: object doesn't exist
;; No other errors - set accepts DottedSymbol directly
```

### `import` Errors
```reactor
(import nonexistent)       ;; ERROR: module not found
;; No other errors - modules are always available once imported
```

## Advanced Patterns

### Functional Object Extension

```reactor
(def create-extended-config
  (lambda (base)
    (lambda ()
      (def base.debug true)
      (def base.features.logging true)
      base  ;; Return extended config
    )))

(def extended ((create-extended-config base-config)))
;; extended has additional properties, base-config unchanged
```

### Module Namespace Management

```reactor
;; Selective importing
(import math.x (cos sin as trig-functions))
trig-functions.cos  ;; → cos function

;; Namespace aliasing
(def trig math.x)
trig.cos  ;; → cos function
```

### Hybrid Data/Code Composition

```reactor
;; Mix data and imported functions
(def ui (:theme "dark"
         :components (:button math.x :input math.y)))

ui.components.button.cos  ;; Access imported function through data structure
```

## Implementation Notes

### AST/IR Representation
- **AST**: `Symbol "x"` → simple variable, `Symbol "x.y.z"` → dotted symbol as string
- **IR**: `Symbol "x"` → simple variable, `DottedSymbol ["x", "y", "z"]` → parsed hierarchical path
- **Compilation**: `Symbol "x.y.z"` → `DottedSymbol ["x", "y", "z"]` (string splitting on ".")
- `String "literal"` → string literals

### Evaluation Strategy
1. **Parse** → Convert dotted notation to appropriate AST nodes
2. **Resolve** → Look up in lexical environment
3. **Apply** → Execute appropriate binding/mutation logic
4. **Return** → Result or error

### Type Safety
- Operations are type-checked at runtime
- Clear error messages for invalid operations
- No implicit conversions between data types

## Summary

Reactor's binding system provides:

- **Clear separation** between definition (`def`), mutation (`set`), and import (`import`)
- **Consistent scoping** with lexical environments
- **Unified composition** through dotted notation for both data and code
- **Type safety** with helpful error messages
- **Functional patterns** for immutable data manipulation

This design enables sophisticated composition patterns while maintaining simplicity and safety.
