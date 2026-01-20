# 🌟 Glue Language Overview

## 🤔 What is Glue?

Glue is a modern, Lisp-inspired programming language designed for embedded scripting, domain-specific languages (DSLs), and functional programming. It combines the elegance of Lisp with contemporary features for property-based objects, modules, and server-driven UI development.

## ✨ Key Characteristics

### 🎯 Simple Yet Powerful
Glue uses a clean, consistent syntax with minimal ceremony. Every construct follows uniform rules, allowing complex operations to emerge from simple, composable primitives.

### 🛡️ Safe by Design
Built-in type checking, immutable data structures by default, and controlled mutation prevent common programming errors. The evaluation model avoids undefined behavior with comprehensive error handling.

### 🔗 Flexible Integration
Designed to embed seamlessly in existing applications with minimal runtime footprint. Clean foreign function interfaces enable integration with host languages while maintaining safety boundaries.

## Core Features

### Functional Programming
```closure
;; First-class functions and closures
(def make-adder (lambda (x) (lambda (y) (+ x y))))
(def add-five (make-adder 5))
(add-five 3)  ;; → 8

;; Universal currying - all functions support partial application
(def add (lambda (a b) (+ a b)))
(def add-five (add 5))  ;; Partial application
(add-five 3)            ;; → 8

;; Built-in functions also curry
(def multiply-by-two (* 2))  ;; Partial application of *
(multiply-by-two 5)          ;; → 10

;; Higher-order functions
(map (lambda (x) (* x 2)) '(1 2 3))  ;; → (2 4 6)
```

### Property-Based Objects
```closure
;; Object creation and manipulation
(def user (:name "Alice" :age 30 :email "alice@example.com"))

;; Property access
user.name      ;; → "Alice"
user.age       ;; → 30

;; Dynamic updates
(set user.age 31)
(set user.location (:city "New York" :country "USA"))
```

### Module System
```closure
;; Module definition and imports
(import math)
(import ui.components)

;; Direct access to exported symbols
sin       ;; Sine function
pi        ;; Pi constant
button    ;; UI component
```

## Syntax Essentials

### Atoms
- **Numbers**: `42`, `3.14159`, `-273.15`
- **Strings**: `"hello world"`, `"with \"quotes\""`
- **Symbols**: `my-variable`, `function-name`, `+`
- **Booleans**: `true`, `false`

### Lists and Calls
```closure
;; Function calls
(+ 1 2 3)              ;; → 6
(* (+ 1 2) (+ 3 4))    ;; → 21

;; Data lists
(1 2 3 4)
("apple" "banana" "cherry")
```

### Property Objects
```closure
;; Object literals
(:name "Alice" :age 30)

;; Nested objects
(:user (:name "Bob" :profile (:theme "dark"))
 :config (:debug true :port 8080))
```

### Control Flow
```closure
;; Conditional execution
(if (> x 0) "positive" "non-positive")

;; Logical operations
(and (> x 0) (< x 100))  ;; → true if 0 < x < 100
```

## Advanced Capabilities

### DSL Creation
Glue excels at creating domain-specific languages through its configurable evaluation environment.

### Server-Driven UI
Built-in support for dynamic UI generation with property-based component configuration and backend integration.

### Embedded Scripting
Minimal footprint and clean host language integration make Glue ideal for embedding business logic in larger applications.

## Hello World Examples

### Simple Script
```closure
(print "Hello, Glue World!")
```

### Functional Programming
```closure
(def factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(factorial 5)  ;; → 120
```

### Object Processing
```closure
(def users ((:name "Alice" :age 30)
            (:name "Bob" :age 25)
            (:name "Charlie" :age 35)))

(def get-names
  (lambda (users)
    (map (lambda (user) user.name) users)))

(get-names users)  ;; → ("Alice" "Bob" "Charlie")
```

### Configuration Management
```closure
(def config (:debug false
              :server (:host "localhost" :port 8080)
              :database (:type "postgres" :name "myapp")))
```

## Why Glue?

### For Developers
- **Familiar Lisp Syntax**: Leverages decades of Lisp wisdom
- **Modern Features**: Functional programming with objects and modules
- **Type Safety**: Runtime checking prevents runtime errors
- **Composability**: Simple primitives combine into complex systems

### For Applications
- **Embedding**: Clean integration with existing codebases
- **Configuration**: Declarative, programmable configuration files
- **DSLs**: Easy creation of domain-specific languages
- **UI Development**: Server-driven UI with dynamic component generation

### For Organizations
- **Safety**: Prevents common programming errors
- **Maintainability**: Clear, consistent code structure
- **Extensibility**: Easy to adapt to new requirements
- **Integration**: Works seamlessly with existing systems

## Getting Started

Follow this learning path to master Glue:

1. **Learn the design philosophy** → [Language Aims](language-aims.md) - Goals and principles behind Glue
2. **Understand how Glue works** → [Execution Pipeline](execution-pipeline.md) - How Glue processes code from text to results
3. **Master the syntax** → [Syntax Reference](syntax.md) - Complete language grammar and constructs
4. **Dive into AST** → [AST Specification](ast.md) - How source code becomes a tree structure
5. **Understand compilation** → [IR Specification](ir.md) - Intermediate representation for execution
6. **Learn evaluation** → [Evaluation](evaluation/README.md) - Runtime execution model and semantics

Glue represents a modern take on Lisp's philosophy: simple, composable primitives that enable powerful abstractions while maintaining safety and usability in real-world applications.
