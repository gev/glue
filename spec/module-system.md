# Module System

## Overview

Glue's module system provides clean separation of code into reusable units with explicit imports and exports. The system uses a dual-registry architecture with eager registration and lazy evaluation, ensuring modules are evaluated once and cached globally.

**Related Documents:**
- [Environment](environment.md) - Variable scoping and binding rules
- [Evaluation Overview](../evaluation/README.md) - Runtime execution semantics

## Key Features

- **Explicit exports**: Only specified symbols are exported from modules
- **Direct imports**: `(import module.name)` brings symbols directly into scope
- **Lazy evaluation with caching**: Modules evaluated once, cached globally
- **Environment isolation**: Modules cannot access external state during loading
- **Lexical scoping**: Imports respect block structure and nesting levels

## Module Declaration

Modules are declared using the `module` special form:

```closure
(module <name>
    (export <symbol> ...)
    <body> ...)
```

- `name`: Module identifier (supports dotted notation: `math.utils`)
- `export`: List of symbols to export (optional, defaults to empty)
- `body`: Module implementation forms

### Example

```closure
(module math.utils
    (export add multiply divide)

    (def add (lambda (a b) (+ a b)))
    (def multiply (lambda (a b) (* a b)))
    (def divide (lambda (a b) (/ a b)))
    (def helper 42)  ; Not exported, private to module
)
```

## Import System

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

### Dual-Access Mechanism

When a module is imported, Glue integrates its exported symbols into the **current local environment frame** through a **dual-access mechanism**:

1. **Direct Symbol Access**: Exported symbols become direct variables in the importing scope
2. **Module Object Access**: The module name provides access to the complete module namespace

### Direct Symbol Integration

Imported symbols are merged directly into the current environment frame:

```closure
(import math)
(+ pi 1)        ;; 'pi' is now accessible as a direct variable
```

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
module.submodule.symbol.object.property.field
```

**Environment Lookup:**
For `DottedSymbol ["base", "prop1", "prop2", ...]`:

1. **Always** search `"base"` in the environment first
2. If found, delegate property access navigation to evaluation
3. See [Evaluation](../evaluation/) for complete dotted symbol resolution algorithm

### Features

- **Direct access**: Imported symbols available without qualification
- **Dotted access**: Hierarchical access through module namespaces
- **Lexical scoping**: Imports work at any nesting level
- **No conflicts**: Multiple modules can be imported safely
- **Caching**: Subsequent imports of same module use cached results

## Architecture

### Dual-Registry Design

The module system uses two separate registries:

#### 1. Module Registry (Registration)

Stores module metadata extracted during the registration phase:
- Module name
- Export list
- Body forms for evaluation

#### 2. Imported Module Cache (Runtime)

Stores evaluated module results for reuse:
- Module name
- Exported symbol values
- Evaluation context

### Two-Phase Process

#### Phase 1: Registration (Eager)

- Parse module declarations from source
- Extract metadata (name, exports, body)
- Store in Module Registry
- No evaluation of module code

#### Phase 2: Import (Lazy with Caching)

- **First import**: Evaluate module body in isolated environment, cache results
- **Subsequent imports**: Return cached results directly

### Environment Isolation

Modules are evaluated in isolated environments containing only:
- Built-in functions and special forms
- Module-specific definitions
- Explicitly imported dependencies

**Security guarantee**: Modules cannot access variables from the importing scope during loading.

#### Example Security Property

```closure
;; Main program
(def secret-password "admin123")

;; Module cannot access external state
(module safe.module
    (export get-password)
    (def get-password (lambda () secret-password)))  ; Error: secret-password not defined

;; Usage
(import safe.module)
(get-password)  ; Runtime error or undefined behavior
```

## Implementation Details

### Module Registration

Module registration occurs during program initialization:

1. Parse module IR structures
2. Extract export lists from `(export ...)` forms
3. Collect body forms for runtime evaluation
4. Store metadata in global registry

### Import Resolution

When `(import module.name)` is evaluated:

1. **Cache check**: Look up module in Imported Cache
2. **First import path**:
   - Retrieve module metadata from Module Registry
   - Create isolated evaluation environment
   - Evaluate module body forms
   - Extract exported symbol values
   - Cache results in Imported Cache
3. **Environment merge**: Add exported symbols and module object to current scope

### Environment Frame Structure After Import

```closure
Before Import:
[user_vars, builtins]

After Import:
[imported_symbols, module_objects, user_vars, builtins]
```

Imported modules create new frames containing both direct symbol access and module object references, maintaining clean separation while providing flexible access patterns.

### Symbol Resolution

Imported symbols are merged into the current environment frame, providing direct access without qualification. The environment maintains lexical scoping rules.

## Error Conditions

- **ModuleNotFound**: Referenced module not in registry
- **InvalidModuleStructure**: Malformed module declaration
- **InvalidExportList**: Non-symbol in export list
- **DuplicateModuleName**: Module name already registered

## File Organization (Optional)

Modules are typically organized in a directory structure:

```
stdlib/
├── core/
│   ├── list.rct      # (module core.list ...)
│   └── math.rct      # (module core.math ...)
└── utils/
    └── string.rct    # (module utils.string ...)
```

## Usage Patterns

### Basic Module Usage

```closure
;; Define module
(module calculator
    (export add subtract)

    (def add (lambda (a b) (+ a b)))
    (def subtract (lambda (a b) (- a b)))
)

;; Use module
(import calculator)

(def result (add 5 (subtract 10 3)))  ; 5 + (10 - 3) = 12
```

### Module Dependencies

```closure
;; Base math module
(module math.basic
    (export add multiply)

    (def add (lambda (a b) (+ a b)))
    (def multiply (lambda (a b) (* a b)))
)

;; Dependent module
(module math.advanced
    (export power)

    (import math.basic)  ; Import dependency

    (def square (lambda (x) (multiply x x)))
    (def power (lambda (base exp)
        (if (= exp 0)
            1
            (multiply base (power base (- exp 1))))))
)
```

## Benefits

1. **Clean interfaces**: Explicit exports prevent accidental dependencies
2. **Efficient loading**: Modules loaded once, cached globally
3. **Scoped imports**: Lexical scoping prevents pollution
4. **Security**: Isolated evaluation prevents external access
5. **Simple implementation**: No complex dependency resolution

## Future Extensions

- **Aliased imports**: `(import math.utils as m)` with alias access
- **Selective imports**: `(import math.utils add multiply)`
- **Documentation extraction**: Embedded documentation in modules
