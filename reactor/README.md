# 🚀 Reactor Language

> A modern, Lisp-inspired programming language with a focus on simplicity, safety, and expressiveness.

[![Haskell](https://img.shields.io/badge/Language-Haskell-5e5086.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](LICENSE)

## ✨ Overview

Reactor is a **embeddable Lisp-inspired scripting language** designed for operating on host language objects and functions. It serves as a universal controller that receives DTOs (Data Transfer Objects) in Reactor syntax and evaluates them using FFI bindings to domain services, constructors, and business logic injected into its lexical environment.

### 🎯 Key Features

- **🔌 Embedded Scripting**: Designed for seamless integration with host applications
- **🏗️ Domain Object Operations**: Manipulate DTOs, dictionaries, GUI components, and business objects
- **🔗 FFI Integration**: Call host language functions and constructors via lexical scope injection
- **🛡️ Safe by Design**: No null pointers, no undefined behavior
- **📝 Lisp Syntax**: Familiar parentheses-based syntax with modern enhancements
- **🔧 Property Objects**: Built-in support for structured data with dot notation access
- **λ Lambda Functions**: First-class functions with lexical scoping
- **📦 Immutable Data**: Pure functional programming paradigm
- **🎨 Quote Sugar**: Convenient `'` syntax for data literals

## 📖 Language Guide

### 🔤 Basic Syntax

#### Atoms (Primitive Values)

```clojure
; Numbers
42
3.14159

; Strings
"hello world"
"multi\nline"

; Symbols (identifiers)
my-variable
function-name
```

#### Lists (Function Calls & Data)

```clojure
; Function call
(+ 1 2 3)  ; → 6

; Nested calls
(* (+ 1 2) (+ 3 4))  ; → 21
```

### 📦 Property Objects

Reactor has built-in support for structured data using property lists:

```clojure
; Create an object
(:name "Alice" :age 30 :active true)

; Access properties
(:name "Alice" :age 30).name    ; → "Alice"
(:name "Alice" :age 30).age     ; → 30

; Property shorthand - these are equivalent:
(f :x 1 :y 2)
(f (:x 1) (:y 2))  ; Same as above
```

### 🔧 Variable Management

```clojure
; Define variables
(def pi 3.14159)
(def greeting "Hello")

; Update variables
(set pi 3.14)
(set user.name "Bob")  ; Update object properties
```

For detailed information about binding semantics, scope rules, and the differences between `def`, `set`, and `import` operations, see [`BINDING_SEMANTICS.md`](BINDING_SEMANTICS.md).

For a complete syntax reference including grammar, operators, and advanced features, see [`SYNTAX.md`](SYNTAX.md).

For technical details about the AST and IR representations, see [`AST.md`](AST.md) and [`IR.md`](IR.md).

### λ Lambda Functions

```clojure
; Define anonymous functions
(lambda (x) (* x x))  ; Square function

; Use lambdas
((lambda (x) (* x x)) 5)  ; → 25

; Store in variables
(def square (lambda (x) (* x x)))
(square 5)  ; → 25

; Multiple parameters
(def add (lambda (a b) (+ a b)))
(add 3 4)  ; → 7

; Closures (lexical scoping)
(def make-adder (lambda (x) (lambda (y) (+ x y))))
(def add-five (make-adder 5))
(add-five 3)  ; → 8
```

### 🎯 Data Literals with Quote

Use `'` (quote) to create data literals instead of function calls:

```clojure
; Without quote - function call
(+ 1 2)  ; → 3

; With quote - data literal
'(+ 1 2)  ; → (+ 1 2) as data

; Quoted objects
'(:name "Alice" :age 30)  ; → {:name "Alice", :age 30}

; Nested quotes
''foo  ; → (quote foo)
```

## 🔌 Embedding & FFI Integration

Reactor's primary purpose is **embedding into host applications** as a universal controller for domain objects, DTOs, and business logic. The interpreter receives Reactor scripts containing DTOs and evaluates them using FFI bindings to host language functions injected into the lexical environment.

### 🏗️ Architecture Pattern

```
Host Application (Haskell/Java/etc.)
    ↓ Injects domain functions into Reactor environment
Reactor Interpreter
    ↓ Receives DTOs in Reactor syntax
    ↓ Evaluates using injected FFI bindings
    ↓ Returns results to host application
```

### 📊 Domain Object Example

```haskell
-- Host application (Haskell)
data User = User { name :: String, age :: Int, email :: String }

createUser :: String -> Int -> String -> IO User
createUser name age email = ...

updateUser :: User -> String -> Int -> IO User
updateUser user newName newAge = ...

-- Inject into Reactor environment
env <- initialEnv
env <- bindFunction env "create-user" createUser
env <- bindFunction env "update-user" updateUser
```

```clojure
; Reactor script (DTO + operations)
(def user-dto (:name "Alice" :age 30 :email "alice@example.com"))

; Use injected host functions
(def user (create-user user-dto.name user-dto.age user-dto.email))
(def updated-user (update-user user "Alice Smith" 31))
```

### 🎮 GUI Component Control

```clojure
; GUI component manipulation
(def button (:id "submit-btn" :text "Submit" :enabled true))

; Update GUI via injected functions
(set-button-text button.id "Processing...")
(set-button-enabled button.id false)

; Event handling
(def handle-click
  (lambda (event)
    (if (validate-form)
        (submit-data (get-form-data))
        (show-error "Validation failed"))))
```

### 🔧 API Controller Pattern

```clojure
; API request DTO
(def request (:method "POST" :path "/users" :body (:name "Bob" :role "admin")))

; Process using injected services
(def user (create-user request.body))
(def response (:status 201 :body user :headers (:content-type "application/json")))

; Error handling
(if (user-exists request.body.name)
    (:status 409 :body "User already exists")
    response)
```

### 📱 Business Logic DSL

```clojure
; Define business rules in Reactor
(def validate-order
  (lambda (order)
    (and
      (> order.amount 0)
      (not (empty? order.items))
      (all (lambda (item) (> item.quantity 0)) order.items))))

(def process-payment
  (lambda (order payment-method)
    (if (validate-order order)
        (charge-card payment-method order.total)
        (throw-error "Invalid order"))))

; Use in host application
result <- evalReactorScript "process-payment" order creditCard
```

### 🖥️ Backend Integration Example

```haskell
-- Haskell backend service
data Order = Order { items :: [String], total :: Double }
data Payment = Payment { method :: String, amount :: Double }

-- Business logic functions
validateOrder :: Order -> Bool
processPayment :: Order -> Payment -> IO Result

-- Inject into Reactor environment
env <- initialEnv
env <- bindFunction env "validate-order" validateOrder
env <- bindFunction env "process-payment" processPayment

-- Reactor script processes API request
reactorScript = "
(def request (:order (:items (\"laptop\" \"mouse\") :total 1299.99)
               :payment (:method \"credit\" :amount 1299.99)))

(if (validate-order request.order)
    (process-payment request.order request.payment)
    (:error \"Invalid order\"))"
```

### 🌐 Frontend Integration Example

```javascript
// JavaScript/React frontend
class OrderProcessor {
  constructor() {
    this.reactor = new ReactorInterpreter();
    // Inject DOM manipulation functions
    this.reactor.bindFunction('update-ui', (state) => {
      this.setState(state);
    });
    this.reactor.bindFunction('validate-form', (data) => {
      return this.validateForm(data);
    });
  }

  async processOrder(orderData) {
    const script = `
      (def order (:items ${JSON.stringify(orderData.items)}
                   :total ${orderData.total}))

      (if (validate-form order)
          (do
            (update-ui (:status "processing"))
            (:result "Order processed"))
          (do
            (update-ui (:error "Validation failed"))
            (:error "Invalid order")))`;

    return await this.reactor.eval(script);
  }
}
```

```

I see the ReactorInterpreter is being used to create a scripting layer that allows dynamic evaluation of order processing logic. The script checks form validation, updates the UI state, and returns appropriate results based on the order's validity. This approach provides a flexible way to handle complex business logic with embedded scripting capabilities.<replace_in_file>
<path>reactor/README.md</path>
<diff>------- SEARCH
; Use in host application
result <- evalReactorScript "process-payment" order creditCard
```

### 🖥️ Backend Integration Example

```haskell
-- Haskell backend service
data Order = Order { items :: [String], total :: Double }
data Payment = Payment { method :: String, amount :: Double }

-- Business logic functions
validateOrder :: Order -> Bool
processPayment :: Order -> Payment -> IO Result

-- Inject into Reactor environment
env <- initialEnv
env <- bindFunction env "validate-order" validateOrder
env <- bindFunction env "process-payment" processPayment

-- Reactor script processes API request
reactorScript = "
(def request (:order (:items (\"laptop\" \"mouse\") :total 1299.99)
               :payment (:method \"credit\" :amount 1299.99)))

(if (validate-order request.order)
    (process-payment request.order request.payment)
    (:error \"Invalid order\"))"
```

### 🌐 Frontend Integration Example

```javascript
// JavaScript/React frontend
class OrderProcessor {
  constructor() {
    this.reactor = new ReactorInterpreter();
    // Inject DOM manipulation functions
    this.reactor.bindFunction('update-ui', (state) => {
      this.setState(state);
    });
    this.reactor.bindFunction('validate-form', (data) => {
      return this.validateForm(data);
    });
  }

  async processOrder(orderData) {
    const script = `
      (def order (:items ${JSON.stringify(orderData.items)}
                   :total ${orderData.total}))

      (if (validate-form order)
          (do
            (update-ui (:status "processing"))
            (:result "Order processed"))
          (do
            (update-ui (:error "Validation failed"))
            (:error "Invalid order")))`;

    return await this.reactor.eval(script);
  }
}
```

## 📚 Advanced Examples

### 🔄 Functional Programming

```clojure
; Higher-order functions
(def apply-twice (lambda (f x) (f (f x))))
(def add-one (lambda (x) (+ x 1)))
(apply-twice add-one 5)  ; → 7

; Function composition
(def compose (lambda (f g) (lambda (x) (f (g x)))))
(def double (lambda (x) (* x 2)))
(def square (lambda (x) (* x x)))
((compose square double) 3)  ; → 36 ((3*2)²)
```

### 📊 Working with Objects

```clojure
; Create user object
(def user (:name "Alice" :age 30 :hobbies ("reading" "coding")))

; Access properties
user.name      ; → "Alice"
user.age       ; → 30
(user.hobbies)  ; → ["reading", "coding"]

; Update properties
(set user.age 31)
(set user.email "alice@example.com")

; Nested objects
(def company (:name "TechCorp" :ceo (:name "Bob" :age 45)))
company.ceo.name  ; → "Bob"
```

### 🔄 Control Flow Patterns

```clojure
; Conditional logic (using built-in functions)
(def is-positive (lambda (x) (> x 0)))

; Function pipelines
(def process-data
  (lambda (data)
    ((validate data)
     (transform data)
     (save data))))

; Error handling patterns
(def safe-divide
  (lambda (a b)
    (if (= b 0)
        "division by zero"
        (/ a b))))
```

## 🏗️ Architecture

### 📁 Project Structure

```
reactor/
├── reactor.cabal     # Haskell package configuration
├── app/
│   └── Main.hs       # REPL/CLI entry point
├── src/
│   ├── Reactor.hs    # Main module
│   └── Reactor/
│       ├── AST.hs        # Abstract Syntax Tree definitions
│       ├── Parser.hs     # Parser implementation
│       ├── Eval.hs       # Evaluator (interpreter)
│       ├── IR.hs         # Intermediate Representation
│       ├── Env.hs        # Environment management
│       ├── Error.hs      # Error types
│       ├── Eval/
│       │   └── Error.hs  # Evaluation errors
│       ├── Lib/
│       │   ├── Builtin.hs    # Built-in functions
│       │   └── Builtin/      # Built-in implementations
│       │       ├── Def.hs
│       │       ├── Set.hs
│       │       ├── List.hs
│       │       ├── Lambda.hs
│       │       ├── Quote.hs
│       │       └── Closure.hs
│       ├── Parser/
│       │   └── Error.hs  # Parser errors
│       └── Spec/
│           └── Device.hs # Device specifications
├── test/
│   ├── Spec.hs       # Test entry point
│   └── Reactor/
│       ├── CompileSpec.hs
│       ├── EnvSpec.hs
│       ├── EvalSpec.hs
│       ├── ParserSpec.hs
│       └── Lib/
│           └── Builtin/
│               ├── DefSpec.hs
│               ├── SetSpec.hs
│               ├── ListSpec.hs
│               ├── LambdaSpec.hs
│               ├── QuoteSpec.hs
│               └── ClosureSpec.hs
```

### 🔄 Evaluation Model

1. **Parse** source code into AST
2. **Compile** AST to IR (Intermediate Representation)
3. **Evaluate** IR in environment with lexical scoping
4. **Return** result or error

### 🛡️ Safety Features

- **Type-safe evaluation** - runtime type checking
- **Lexical scoping** - proper variable isolation
- **Comprehensive error handling** - detailed error messages


## 📖 Grammar Reference

### EBNF Grammar

```
program     ::= expr

expr        ::= atom
              | list
              | prop_list
              | prop_access
              | quoted_expr

atom        ::= number | string | symbol

list        ::= "(" expr* ")"

prop_list   ::= "(" (":" symbol expr)* ")"

prop_access ::= expr "." symbol

quoted_expr ::= "'" expr

number      ::= digit+ ["." digit+]

string      ::= '"' char* '"'

symbol      ::= letter (letter | digit | "-" | "_")*
```

### Operator Precedence

- **Left to right** evaluation
- **No operator precedence** - use explicit grouping
- **Function application** has highest precedence

## 🎨 Design Philosophy

### 💡 Why Reactor?

1. **Simplicity**: Minimal syntax, maximal expressiveness
2. **Safety**: Designed to prevent common programming errors
3. **Flexibility**: Easy to extend and embed
4. **Modern**: Combines Lisp elegance with contemporary features

### 🎯 Use Cases

#### 🖥️ **Backend Applications**

- **🔌 Embedded Scripting**: Universal controller for domain objects and DTOs
- **🏗️ Domain Logic**: Manipulate business objects, services, and database operations
- **🔗 FFI Integration**: Call host language functions via injected environment
- **📊 Data Processing**: Transform DTOs and dictionaries functionally
- **⚙️ Configuration**: Programmable config with domain object manipulation
- **🔧 DSL Creation**: Custom languages for specific business domains
- **📱 API Controllers**: Process and transform API request/response DTOs
- **🔄 Middleware**: Request/response processing and validation
- **💾 Database Operations**: Query building and result transformation

#### 🌐 **Frontend Applications**

- **🎮 GUI Scripting**: Control UI components and event handling
- **📊 State Management**: Functional state transformations and updates
- **🔄 Data Binding**: Reactive data flow and component communication
- **🎯 Form Validation**: Declarative validation rules and error handling
- **📱 Client-side Logic**: Business rules execution in the browser
- **🔄 Event Processing**: User interaction handling and side effects
- **🎨 Component Logic**: UI component behavior and state management

#### 🏗️ **CAD & Design Systems**

- **📐 Geometric Modeling**: Manipulate 3D objects, meshes, and parametric designs
- **🔧 Automation Scripts**: Batch processing and design rule validation
- **📊 Parametric Design**: Generate variations using functional transformations
- **🔄 Workflow Automation**: Custom design pipelines and constraint solving
- **📏 Measurement & Analysis**: Geometric calculations and property extraction
- **🔗 Plugin Integration**: Extend CAD functionality with custom logic
- **🎯 Design Validation**: Rule-based checking and quality assurance

## 🤝 Contributing

We welcome contributions! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

BSD 3-Clause License - see LICENSE file for details.

## 🙏 Acknowledgments

- Inspired by Lisp family of languages
- Built with Haskell for reliability and performance
- Designed for the ReactHome IoT platform

---

Happy coding with Reactor! 🎉
