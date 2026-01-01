# Language Overview

## Introduction

Reactor is a Lisp-inspired scripting language designed for embedding in host applications. It provides a functional programming paradigm with modern enhancements for property objects, modules, and foreign function interfaces.

## Core Components

### Syntax
Reactor uses a parentheses-based syntax similar to Lisp, with enhancements for property objects and data literals.

**Basic Expressions:**
```reactor
;; Function calls
(+ 1 2 3)              ;; → 6

;; Property objects
(:name "Alice" :age 30)

;; Property access
user.name

;; Data literals
'(1 2 3)               ;; Quoted list
```

### Dynamic Environment
Reactor maintains a dynamic lexical environment for variable binding and lookup.

- **Lexical Scoping**: Variables are resolved in the lexical context where they are defined
- **Dynamic Binding**: Environment can be modified at runtime
- **Nested Environments**: Support for closures and nested scopes

### Module System
Modules provide namespacing and code organization.

- **Module Loading**: Import modules to access their exports
- **Namespace Isolation**: Prevent name conflicts between modules
- **Dynamic Registration**: Modules can be loaded and registered at runtime

### Foreign Function Interface (FFI)
FFI enables calling host language functions from Reactor scripts.

- **Function Injection**: Host functions are injected into the Reactor environment
- **Type Marshalling**: Automatic conversion between Reactor and host types
- **Bidirectional Communication**: Call host functions and return results to Reactor

## Evaluation Model

Reactor follows a three-phase evaluation process:

1. **Parse** source code into Abstract Syntax Tree (AST)
2. **Compile** AST to Intermediate Representation (IR)
3. **Evaluate** IR in the dynamic environment

## Data Types

### Primitive Types
- **Numbers**: Integers and floating-point numbers
- **Strings**: Text data with escape sequences
- **Symbols**: Identifiers for variables and functions
- **Booleans**: `true` and `false` values

### Composite Types
- **Lists**: Ordered sequences of values
- **Property Objects**: Key-value dictionaries with dot notation access
- **Functions**: First-class functions with lexical closures

## Control Flow

### Conditional Execution
```reactor
(if condition
    true-branch
    false-branch)
```

### Logical Operators
```reactor
(and condition1 condition2)
(or condition1 condition2)
(not condition)
```

## Special Forms

Reactor provides special forms for control flow and evaluation:
- `if` - conditional execution
- `lambda` - function creation
- `quote` - data literals
- `def` - variable definition
- `set` - variable mutation
- `import` - module loading

## Examples

### Hello World
```reactor
(print "Hello, World!")
```

### Function Definition and Call
```reactor
(def square (lambda (x) (* x x)))
(square 5)  ;; → 25
```

### Object Manipulation
```reactor
(def user (:name "Alice" :age 30))
user.name   ;; → "Alice"
(set user.age 31)
```

### Module Usage
```reactor
(import math)
(math.sqrt 16)  ;; → 4
```

This overview provides the foundation for understanding Reactor's design and capabilities. Subsequent documents provide detailed specifications for each component.
