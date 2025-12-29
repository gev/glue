# Reactor Module System

## Overview

Reactor implements a module system inspired by OCaml, providing clean separation of code into reusable units with explicit imports and exports.

## Key Features

- **OCaml-style imports**: `(import module.name)` brings symbols directly into scope
- **Explicit exports**: Only specified symbols are exported from modules
- **Eager registration, lazy evaluation**: Modules are registered upfront but evaluated only when imported
- **Lexical scoping**: Imports respect block structure and nesting levels

## Module Declaration

```clojure
(module math.utils
    (export add multiply divide)

    (def add (lambda (a b) (+ a b)))
    (def multiply (lambda (a b) (* a b)))
    (def divide (lambda (a b) (/ a b)))
    (def helper 42)  ; Not exported
)
```

### Syntax
```
(module <name>
    (export <symbol> ...)
    <body> ...)
```

- `name`: Module identifier (supports dots: `math.utils`)
- `export`: List of symbols to export
- `body`: Module implementation (defs, etc.)

## Import System

```clojure
(import math.utils)  ; Brings add, multiply, divide into scope

(add 1 2)           ; Available directly
(multiply 3 4)      ; Available directly
```

### Features
- **Direct access**: Imported symbols available without qualification
- **Lexical scoping**: Imports work at any nesting level
- **No conflicts**: Multiple modules can be imported safely

## Architecture

### Two-Phase Process

#### 1. Registration Phase
- Parse all module IR (pre-compiled)
- Extract module structure (name, exports, body)
- Store in global module registry
- **No evaluation** of module code

#### 2. Runtime Phase
- `(import module.name)` triggers module loading
- Evaluate module body in isolated environment
- Extract exported symbol values
- Merge into current scope

### Environments

#### Registration Environment
Contains special forms for parsing modules:
- `module` - Parse module declarations
- `export` - Collect export lists
- `def` - Record symbol definitions (no evaluation)

#### Runtime Environment
Contains execution forms:
- `import` - Load and merge modules
- `def` - Evaluate and define variables
- All standard builtins

## Implementation Details

### Module Data Structure
```haskell
data Module = Module
    { moduleName :: Text
    , exportedSymbols :: [Text]     -- Symbol names only
    , body :: [IR]                  -- IR forms for evaluation
    }
```

### Registry
```haskell
type ModuleRegistry = Map Text Module
```

### Special Forms

#### `module` (Registration)
- Parses module structure
- Records metadata in registry
- Stores body for later evaluation

#### `export` (Registration)
- Collects symbol names for export
- Only works during module registration

#### `import` (Runtime)
- Looks up module in registry
- Evaluates module body
- Merges exported symbols into current environment

## File Organization

```
stdlib/
├── core/
│   ├── list.r      # (module core.list ...)
│   └── math.r      # (module core.math ...)
└── utils/
    └── string.r    # (module utils.string ...)
```

## Example Usage

### Module Definition
```clojure
;; math.r
(module math.basic
    (export add multiply)

    (def add (lambda (a b) (+ a b)))
    (def multiply (lambda (a b) (* a b)))
    (def internal-helper 42)  ; Private
)
```

### Usage
```clojure
;; main.r
(import math.basic)

(def result (add 10 (multiply 2 3)))  ; 10 + (2*3) = 16
```

## Benefits

1. **Clean interfaces**: Explicit exports prevent accidental dependencies
2. **Flexible imports**: OCaml-style unqualified imports
3. **Efficient loading**: Modules loaded once, cached globally
4. **Scoped imports**: Lexical scoping prevents pollution
5. **Simple implementation**: No complex dependency resolution

## Comparison to Other Systems

| Feature | Reactor | Python | JavaScript | OCaml |
|---------|---------|--------|------------|-------|
| Import style | Direct | `import`/`from` | `import` | `open` |
| Export control | Explicit | Implicit | Explicit | Explicit |
| Scoping | Lexical | Global | Module | Lexical |
| Loading | Eager reg, lazy eval | Lazy | Lazy | Eager |

## Future Extensions

- **Qualified imports**: `(import math.basic as m)`
- **Selective imports**: `(import (add multiply) from math.basic)`
- **Documentation**: `(doc "Description" (def func ...))`
- **Versioning**: Module version specifications
