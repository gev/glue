# 🚀 Reactor Language Ecosystem Manifest

> A self-contained, portable Lisp-inspired programming language with modular standard library, built-in testing, and cross-implementation compatibility.

## 📋 Table of Contents
- [Vision](#-vision)
- [Architecture](#-architecture)
- [Module System](#-module-system)
- [FFI Framework](#-ffi-framework)
- [Documentation System](#-documentation-system)
- [Testing Framework](#-testing-framework)
- [Standard Library](#-standard-library)
- [Implementation Roadmap](#-implementation-roadmap)
- [Host Language Implementations](#-host-language-implementations)
- [Cross-Implementation Testing](#-cross-implementation-testing)

## 🎯 Vision

Reactor aims to be a **truly portable, self-sustaining programming language ecosystem** where:

1. **Language Core**: Minimal interpreter implemented in multiple host languages
2. **Standard Library**: Pure Reactor modules calling FFI primitives
3. **Testing**: Comprehensive test suite written in Reactor itself
4. **Documentation**: Self-documenting code with generated docs
5. **Modules**: Import/export system for code organization

This creates a language that is **implementation-agnostic** - the same programs run identically across Haskell, Dart, JavaScript, and other host languages.

## ⚙️ Configuration & Data Formats

Beyond being a programming language, Reactor serves as an excellent **configuration and data description language**, providing a more powerful and expressive alternative to JSON, XML, and YAML.

### Dictyanaris Configuration Language

Reactor uses **only parentheses `()`** for its syntax, following traditional Lisp conventions. This provides a uniform, programmable approach to configuration:

```clojure
;; Config using Reactor syntax - only parentheses
(config
    (version "1.0.0")
    (services
        (web-server
            (:port 8080
             :host "0.0.0.0"
             :routes ( (:method "GET" :path "/" :handler "home")
                       (:method "POST" :path "/api/users"
                        :handler "create-user"
                        :middleware (auth logging)))))
        (database
            (:type "postgresql"
             :connection (:host "localhost"
                           :port 5432
                           :database "myapp"
                           :credentials (:username (env "DB_USER")
                                         :password (env "DB_PASS")))))
        (:features (:authentication (:enabled true
                                    :providers (github google local))
                   :caching (:redis (:host "redis:6379"
                                     :ttl 3600))))))
```

**Key Features:**
- **Property objects**: `(:key1 value1 :key2 value2)` for structured data
- **List construction**: `(item1 item2)` for arrays
- **Property access**: `object.key` for accessing properties
- **Programmable**: Embed logic, references, and computations
- **Composable**: Mix and match config fragments
- **Type-safe**: Rich type system prevents config errors

**Advantages over JSON/YAML:**
- **Programmable**: Embed logic, references, and computations
- **Reusable**: Define functions and macros for config patterns
- **Type-safe**: Rich type system prevents config errors
- **Composable**: Mix and match config fragments
- **Self-documenting**: Embed documentation in config itself
- **Uniform syntax**: Everything uses parentheses - no syntax switching

### RPC Data Transfer Objects

Reactor's object and list syntax provides a natural way to define DTOs:

```clojure
;; RPC request/response DTOs
(define-dto user-profile
    (:id (string "UUID")
     :username (string "unique username")
     :email (string "valid email")
     :profile (:first-name string
               :last-name string
               :avatar-url (optional string))
     :preferences (:theme (enum "light" "dark" "auto")
                  :notifications (:email boolean
                                 :push boolean))
     :created-at timestamp
     :updated-at timestamp))

;; API endpoint definition
(define-endpoint get-user-profile
    (:method "GET"
     :path "/users/{id}/profile"
     :params (:id uuid)
     :response user-profile
     :errors ((:code 404 :message "User not found")
              (:code 403 :message "Access denied"))))
```

**Benefits for RPC:**
- **Type Definitions**: Rich type system with validation
- **Schema Evolution**: Easy to version and migrate DTOs
- **Code Generation**: Generate client/server code from DTOs
- **Runtime Validation**: Validate data at boundaries
- **Documentation**: Self-documenting API contracts
- **Command Verbs**: Natural language for device control and automation

### Device Control Commands

Reactor's syntax is ideal for IoT and home automation commands:

```clojure
;; Device control commands using Reactor verbs
(define-command turn-on
    (:device-id (string "device-uuid")
     :target "light"
     :zone "living-room"))

(define-command turn-off
    (:device-id (string "device-uuid")
     :target "light"
     :zone "living-room"))

(define-command setpoint
    (:device-id (string "thermostat-uuid")
     :target "temperature"
     :value (number "degrees")
     :unit "celsius"))

;; Command execution
(execute-command
    (turn-on :device-id "light-001" :zone "kitchen"))

(execute-command
    (setpoint :device-id "thermostat-001"
              :value 22.5
              :unit "celsius"))

;; Batch operations
(execute-commands (
    (turn-off :device-id "light-001" :zone "living-room")
    (turn-on :device-id "light-002" :zone "bedroom")
    (setpoint :device-id "thermostat-001" :value 20.0)))
```

This approach makes device control feel natural and programmable, enabling complex automation scenarios while maintaining type safety and validation.

### Configuration as Code

Reactor enables **Configuration as Code** paradigms:

```clojure
;; Environment-specific config composition
(def base-config
    (:app-name "MyApp"
     :version "1.0.0"))

(def dev-config
    (merge base-config
        (:env "development"
         :debug true
         :database (:host "localhost"))))

(def prod-config
    (merge base-config
        (:env "production"
         :debug false
         :database (:host (env "DB_HOST")
                   :replicas 3))))

;; Select config based on environment
(def current-config
    (cond
        ((= (env "NODE_ENV") "production") prod-config)
        (else dev-config)))
```

This approach provides the power of programming languages for configuration while maintaining the simplicity needed for ops/deployment scenarios.

## 🏗️ Architecture

```
Reactor Ecosystem
├── Core Language (per host language)
│   ├── Parser (Text → AST)
│   ├── AST Builder API (Programmatic AST Construction)
│   ├── AST → IR Compiler
│   ├── Evaluator (IR → Result)
│   ├── Environment (lexical scoping)
│   └── FFI Bindings (host ↔ Reactor)
├── Standard Library (pure Reactor)
│   ├── Standard modules (List, Math, Bool, String)
│   ├── Utility modules (IO, Time, Random)
│   └── User modules (extensible)
├── Tooling & Integration
│   ├── GUI Code Builders
│   ├── AST Transformation APIs
│   ├── Code Generators
│   └── IDE Integration
├── Testing Framework (pure Reactor)
│   ├── Test runner
│   ├── Assertions
│   └── Property testing
└── Documentation System
    ├── Embedded docs
    ├── Doc generation
    └── API reference
```

### AST Construction: Beyond Parentheses

Reactor breaks free from the traditional Lisp limitation of **only round parentheses `()`** for syntax. While text parsing with `()` remains one input method, Reactor's AST can be constructed through multiple pathways:

#### 1. Text Parsing (Traditional)
```haskell
parseReactor :: Text -> Either ParserError AST
parseReactor "(def x (+ 1 2))"  -- Text with () syntax
```

#### 2. Programmatic AST Construction
```haskell
-- Direct AST node creation
ast :: AST
ast = Def "x" (Call "+" [Number 1, Number 2])

-- Builder API for complex structures
program :: AST
program = buildProgram [
    def "factorial" $ lambda ["n"] $
        if' (equal (var "n") (number 0))
            (number 1)
            (mul (var "n") (call "factorial" [sub (var "n") (number 1)]))
]
```

#### 3. GUI-Based Construction
```haskell
-- Hypothetical GUI builder API
guiProgram :: IO AST
guiProgram = do
    -- User drags "function definition" block
    funcDef <- createFunctionDef "greet"
    -- Adds parameter input
    addParameter funcDef "name"
    -- Adds print statement
    addStatement funcDef (printCall (concat (string "Hello, ") (var "name")))
    -- Returns complete AST
    buildAST funcDef
```

#### 4. AST Transformation & Generation
```haskell
-- Macro expansion
expandMacro :: AST -> AST

-- Code generation from schemas
generateDTO :: Schema -> AST

-- Optimization passes
optimizeAST :: AST -> AST
```

### Benefits of Programmatic AST Construction

- **GUI Code Builders**: Visual programming interfaces for Reactor
- **Code Generation**: Generate Reactor code from schemas, APIs, databases
- **Macros & DSLs**: Build domain-specific languages on top of Reactor
- **Tool Integration**: IDEs, editors, and other tools can construct code
- **AST Transformations**: Refactoring, optimization, and code analysis tools
- **Cross-Language Interop**: Convert between Reactor and other language ASTs

### Implementation Strategy

Each host language implementation provides:
- **AST Builder Library**: Constructors for all AST node types
- **Validation**: Type checking during AST construction
- **Serialization**: Convert AST back to text for storage/debugging
- **Transformation APIs**: Tools for AST manipulation and analysis

This multi-modal AST construction enables Reactor to be used not just as a textual programming language, but as a programmable foundation for code generation, GUI builders, and sophisticated development tools.

## 🎛️ Environment-Controlled Language Features

Reactor's language capabilities are controlled through the **environment configuration**, allowing different execution contexts to have different language features. This enables:

- **Modular Language Design**: Core language can be extended with optional features
- **Security**: Restrict available operations in sandboxed environments
- **Compatibility**: Different implementations can support different feature sets
- **Evolution**: Add new features without breaking existing code

### Core Language Features (Always Available)
- **Literals**: Numbers, strings, lists, objects
- **Symbols**: Variable references and function calls
- **Basic Evaluation**: Expression evaluation and result return

### Environment-Configurable Special Forms
Special forms are registered in the environment and can be enabled/disabled per context:

#### Core Special Forms (Always Available)

- **`list`**: Constructs lists from arguments
- **`object`**: Creates objects from key-value pairs

#### Optional Special Forms
- **`def`**: Variable and function definition
- **`lambda` / `\`**: Function creation
- **`set`**: Variable reassignment
- **`module`**: Module declaration with import/export
- **`doc`**: Documentation attachment to expressions

#### Control Flow Special Forms
- **`if`**: Conditional evaluation
- **`cond`**: Multi-branch conditional
- **`when` / `unless`**: Conditional execution
- **`while` / `until`**: Looping constructs
- **`for-each`**: List iteration

### Environment Configuration Examples

**Full-Featured Environment:**
```haskell
fullEnv :: Frame Eval
fullEnv = builtin ++ [
    ("def", Native (Special def)),
    ("lambda", Native (Special lambda)),
    ("\\", Native (Special lambda)),
    ("module", Native (Special moduleSpecial)),
    ("doc", Native (Special docSpecial)),
    ("if", Native (Special ifSpecial)),
    -- ... all features enabled
]
```

**Restricted/Sandboxed Environment:**
```haskell
sandboxEnv :: Frame Eval
sandboxEnv = builtin ++ [
    ("def", Native (Special def)),
    ("lambda", Native (Special lambda)),
    -- no module, doc, or I/O features
]
```

**Minimal Environment:**
```haskell
minimalEnv :: Frame Eval
minimalEnv = builtin  -- only list, object
```

This design enables Reactor to be used in diverse contexts:
- **Full development**: All features available
- **Embedded scripting**: Limited to safe operations
- **Educational**: Progressive feature unlocking
- **Cross-platform**: Feature parity across implementations

## 📦 Module System

### Syntax
```clojure
(module math.arithmetic
    (import ffi-math)
    (export + - * /)

    (def + (lambda (a b)
        (doc "Add two numbers"
             :params (:a "First number" :b "Second number")
             :returns "Sum of a and b"
             :examples ("(+ 1 2) ; => 3"))
        (ffi-add a b)))

    (def - (\ (a b) (ffi-sub a b)))
    (def * (\ (a b) (ffi-mul a b)))
    (def / (\ (a b) (ffi-div a b)))
)

(module my-app
    (import math.arithmetic list)
    (export main)

    (def main ()
        (print (+ 1 (length '(1 2 3)))))
)
```

### Features
- **Import/Export**: Selective symbol exposure
- **File-based**: Load modules from filesystem with hierarchical paths
- **Caching**: Module registry with dependency resolution
- **Namespaces**: Hierarchical module names mapping to filesystem paths

### File System Mapping
Module names directly correspond to file system paths (similar to Haskell):

```
reactor/
├── stdlib/
│   ├── core/
│   │   ├── list.r        # (module core.list ...)
│   │   ├── math.r        # (module core.math ...)
│   │   └── string.r      # (module core.string ...)
│   ├── utils/
│   │   ├── io.r          # (module utils.io ...)
│   │   └── time.r        # (module utils.time ...)
│   └── math/
│       ├── arithmetic.r  # (module math.arithmetic ...)
│       └── trigonometry.r # (module math.trigonometry ...)
└── user/
    └── my-app.r          # (module user.my-app ...)
```

**Module Resolution**: `math.arithmetic` → `stdlib/math/arithmetic.r`

## 🔗 FFI Framework

### Design Principles
1. **Minimal Interface**: Each host language implements ~100 primitive functions
2. **Type Safety**: Strong typing at FFI boundaries
3. **Performance**: Direct host language calls for hot paths
4. **Extensibility**: Easy to add new FFI functions

### Required FFI Functions

#### Core Types
- `ffi-type-of` - Get type of value
- `ffi-equal` - Deep equality comparison
- `ffi-to-string` - Convert to string representation

#### Numbers & Math
- `ffi-add`, `ffi-sub`, `ffi-mul`, `ffi-div`
- `ffi-sin`, `ffi-cos`, `ffi-sqrt`, etc.
- `ffi-parse-number`, `ffi-is-number`

#### Lists & Collections
- `ffi-car`, `ffi-cdr`, `ffi-cons`
- `ffi-length`, `ffi-nth`, `ffi-slice`
- `ffi-map`, `ffi-filter`, `ffi-reduce`

#### Strings
- `ffi-string-length`, `ffi-string-concat`
- `ffi-string-substring`, `ffi-string-split`
- `ffi-string-to-upper`, `ffi-string-to-lower`

#### Objects & Properties
- `ffi-object-create`, `ffi-object-get`, `ffi-object-set`
- `ffi-object-keys`, `ffi-object-has-key`

#### Control Flow
- `ffi-throw`, `ffi-catch`
- `ffi-call-function`, `ffi-apply-function`

#### I/O & System
- `ffi-print`, `ffi-read-line`
- `ffi-file-read`, `ffi-file-write`
- `ffi-current-time`, `ffi-random`

## 📚 Documentation System

### Embedded Documentation
```clojure
(def map (\ (f list)
    (doc "Apply function to each element of list"
         :category "List operations"
         :params (:f "Function to apply" :list "Input list")
         :returns "New list with function applied to each element"
         :examples (
             "(map (lambda (x) (* x 2)) '(1 2 3)) ; => (2 4 6)"
             "(map car '((1 2) (3 4) (5 6))) ; => (1 3 5)"
         )
         :throws ("Error if f is not callable" "Error if list is not a list")
         :see-also ("filter" "reduce" "for-each"))
    (if (empty? list)
        '()
        (cons (f (car list)) (map f (cdr list))))))
```

### Documentation Features
- **Rich Metadata**: Parameters, return types, examples, errors
- **Cross-references**: See-also links between functions
- **Categories**: Group related functions
- **Search**: Query documentation programmatically
- **Generation**: HTML/markdown output

## 🧪 Testing Framework

### Built-in Testing
```clojure
(module test-framework
    (export deftest assert is describe)

    (def deftest (\ (name test-fn)
        (doc "Define a test case")
        ; Register test in global test registry
    ))

    (def assert (\ (condition message)
        (if (not condition)
            (throw (:type "assertion-error" :message message)))))
)

(module math-tests
    (import test-framework math)

    (deftest test-addition
        (assert (= (+ 1 2) 3) "1 + 2 should equal 3")
        (assert (= (+ 0 0) 0) "0 + 0 should equal 0"))

    (deftest test-multiplication
        (assert (= (* 3 4) 12) "3 * 4 should equal 12"))
)
```

### Testing Features
- **Unit Tests**: Individual function testing
- **Integration Tests**: Module interaction testing
- **Property Testing**: Generated test cases
- **Test Discovery**: Automatic test collection
- **Reporting**: Detailed test results and coverage

## 📚 Standard Library

### Standard Modules

#### `core.list`
- **Basic**: `car`, `cdr`, `cons`, `length`, `empty?`
- **Manipulation**: `append`, `reverse`, `take`, `drop`, `slice`
- **Search**: `member?`, `find`, `position`, `nth`
- **Transform**: `map`, `filter`, `reduce`, `flatten`
- **Sort**: `sort`, `sort-by`

#### `core.math`
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`, `pow`
- **Trigonometry**: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
- **Logarithms**: `ln`, `log`, `lg`
- **Utilities**: `abs`, `ceil`, `floor`, `round`, `trunc`, `min`, `max`

#### `core.bool`
- **Logic**: `and`, `or`, `not`, `xor`
- **Comparison**: `=`, `!=`, `<`, `<=`, `>`, `>=`
- **Control**: `if`, `cond`, `when`, `unless`
- **Loops**: `while`, `until`, `for-each`

#### `core.string`
- **Basic**: `length`, `concat`, `substring`, `split`
- **Case**: `to-upper`, `to-lower`, `capitalize`
- **Search**: `contains?`, `starts-with?`, `ends-with?`, `index-of`
- **Transform**: `trim`, `replace`, `join`

### Utility Modules

#### `core.io`
- **Console**: `print`, `println`, `read-line`
- **Files**: `read-file`, `write-file`, `append-file`
- **Paths**: `basename`, `dirname`, `extname`

#### `core.time`
- **Current**: `now`, `current-time`
- **Format**: `format-time`, `parse-time`
- **Arithmetic**: `add-seconds`, `add-minutes`, `add-hours`

#### `core.random`
- **Numbers**: `random`, `random-int`, `random-float`
- **Collections**: `shuffle`, `sample`, `choice`

## 🚀 Implementation Roadmap

### Phase 1: Language Extensions (Q1 2025)
- [ ] Extend AST/IR for modules and FFI
- [ ] Add module loading system
- [ ] Implement FFI calling syntax
- [ ] Create documentation parser

### Phase 2: FFI Framework (Q1 2025)
- [ ] Define complete FFI interface
- [ ] Implement Haskell FFI bindings
- [ ] Create FFI binding generator
- [ ] Test FFI integration

### Phase 3: Reactor Standard Library (Q2 2025)
- [ ] Port all current Lib functions to Reactor
- [ ] Implement core modules (List, Math, Bool, String)
- [ ] Add comprehensive documentation
- [ ] Create module dependency management

### Phase 4: Testing & Documentation (Q2 2025)
- [ ] Build test framework in Reactor
- [ ] Port existing tests to Reactor syntax
- [ ] Implement documentation generator
- [ ] Create cross-implementation test runner

### Phase 5: Additional Implementations (Q3 2025)
- [ ] Dart implementation
- [ ] JavaScript implementation
- [ ] Python implementation (optional)

## 💻 Host Language Implementations

### Haskell (Reference Implementation)
- **Status**: Complete core, partial stdlib
- **FFI**: Direct function bindings
- **Performance**: Optimized for speed
- **Testing**: Full test coverage

### Dart Implementation
- **Status**: Planned
- **FFI**: Dart interop with JavaScript core
- **Target**: Web/mobile applications
- **Integration**: Flutter/React Native

### JavaScript Implementation
- **Status**: Planned
- **FFI**: Direct JavaScript bindings
- **Target**: Web browsers, Node.js
- **Integration**: NPM ecosystem

## 🔄 Cross-Implementation Testing

### Test Categories
1. **Core Language Tests**: Parser, evaluator, environment
2. **FFI Tests**: Host language binding verification
3. **Stdlib Tests**: Standard library functionality
4. **Integration Tests**: End-to-end program execution

### Test Runner Architecture
```
Cross-Platform Test Suite
├── test-definitions/     # Reactor test files
├── implementations/      # Host language runners
│   ├── haskell/
│   ├── dart/
│   └── javascript/
└── results/             # Test results comparison
```

### Compatibility Requirements
- **Behavioral Consistency**: Same results across implementations
- **Error Handling**: Identical error messages and codes
- **Performance**: Within 2x of reference implementation
- **FFI Coverage**: All required primitives implemented

## 🎯 Success Metrics

### Language Maturity
- [ ] 100+ built-in functions across all implementations
- [ ] Comprehensive test suite (>1000 tests)
- [ ] Generated documentation for all functions
- [ ] Module system with dependency resolution

### Ecosystem Health
- [ ] 3+ host language implementations
- [ ] Cross-implementation test compatibility
- [ ] Active community and contribution guidelines
- [ ] Performance benchmarks vs. similar languages

### Adoption Goals
- [ ] Production-ready for scripting tasks
- [ ] Integration with major frameworks
- [ ] Educational use in programming courses
- [ ] Commercial applications and tooling

---

## 🤝 Contributing

This manifest serves as the roadmap for Reactor's evolution. Contributions are welcome in:

- Core language implementations
- Standard library modules
- Testing and documentation
- Tooling and developer experience

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## 📄 License

BSD 3-Clause License - see LICENSE file for details.

---

*This document represents the current vision for Reactor. As the project evolves, this manifest will be updated to reflect new insights and priorities.*
