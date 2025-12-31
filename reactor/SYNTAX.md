# Reactor Syntax Reference

## Overview

Reactor uses a Lisp-inspired syntax with modern enhancements for property objects, functional programming, and module systems. This document provides a complete reference of Reactor's syntax and grammar.

## Basic Elements

### Atoms (Primitive Values)

#### Numbers
```reactor
42          ;; Integer
3.14159     ;; Float
-273.15     ;; Negative float
```

#### Strings
```reactor
"hello world"
"multi\nline\tstring"
"quote: \" and backslash: \\"
```

#### Symbols (Identifiers)
```reactor
my-variable
function-name
x
y
math.pi
```

#### Booleans
```reactor
true
false
```

### Comments
```reactor
;; Single line comment

;; Multi-line comments
;; are just multiple single-line comments

(+ 1 2)  ;; Inline comment
```

## Lists (Function Calls & Data Structures)

### Function Calls
```reactor
;; Basic function call
(+ 1 2 3)              ;; → 6

;; Nested calls
(* (+ 1 2) (+ 3 4))    ;; → 21

;; No arguments
(now)                   ;; → current timestamp

;; Single argument
(negate -5)             ;; → 5
```

### Data Lists
```reactor
;; List literals
'(1 2 3 4)
'("apple" "banana" "cherry")

;; Quoted expressions (data, not code)
'(+ 1 2)                ;; → (+ 1 2) as data
(+ 1 2)                 ;; → 3 as code
```

## Property Objects

### Basic Object Creation
```reactor
;; Empty object
(:)

;; Simple properties
(:name "Alice" :age 30)

;; Nested objects
(:user (:name "Bob" :age 25)
 :config (:theme "dark" :lang "en"))

;; Computed values
(:result (* 2 21) :timestamp (now))
```

### Property Access
```reactor
;; Direct property access
(:name "Alice").name           ;; → "Alice"

;; Nested access
(:user (:name "Bob")).user.name  ;; → "Bob"

;; Dotted notation (equivalent)
user.name                      ;; → property access
user.address.city              ;; → nested access
```

### Property Update
```reactor
;; Create object
(def user (:name "Alice" :age 30))

;; Update properties
(set user.age 31)
(set user.email "alice@example.com")

;; Result: (:name "Alice" :age 31 :email "alice@example.com")
```

## Variable Management

### Definition (`def`)
```reactor
;; Simple variables
(def x 42)
(def greeting "Hello")

;; Object creation
(def config (:debug true :port 8080))

;; Local object extension
(def config.timeout 5000)      ;; Extends config locally
```

### Mutation (`set`)
```reactor
;; Variable update
(set x 100)

;; Property update
(set user.name "Bob")

;; Deep property update
(set config.database.host "localhost")
```

### Import (`import`)
```reactor
;; Module import
(import math.x)         ;; Import specific module
(import math)           ;; Import namespace (if supported)

;; Selective import (if supported)
(import math.x (cos sin as trig))
```

## Functions

### Lambda Functions
```reactor
;; Anonymous function
(lambda (x) (* x x))

;; Multiple parameters
(lambda (a b) (+ a b))

;; No parameters
(lambda () "hello")

;; Closures
(def make-adder (lambda (x) (lambda (y) (+ x y))))
(def add-five (make-adder 5))
(add-five 3)  ;; → 8
```

### Function Application
```reactor
;; Direct call
((lambda (x) (* x 2)) 5)  ;; → 10

;; Stored function
(def double (lambda (x) (* x 2)))
(double 21)  ;; → 42

;; Higher-order functions
(def apply-twice (lambda (f x) (f (f x))))
(apply-twice double 5)  ;; → 20
```

## Control Flow

### Conditional (`if`)
```reactor
;; Basic conditional
(if (> x 0) "positive" "non-positive")

;; With else
(if (even? x)
    "even"
    "odd")

;; Nested conditionals
(if (> x 0)
    (if (< x 10) "small positive" "large positive")
    "non-positive")
```

### Logical Operators
```reactor
;; And
(and (> x 0) (< x 100))  ;; → true if 0 < x < 100

;; Or
(or (= x 0) (= x 1))     ;; → true if x is 0 or 1

;; Not
(! (= x 0))              ;; → true if x is not 0
```

## Data Structures

### Lists
```reactor
;; List creation
(list 1 2 3 4)
'(1 2 3 4)               ;; Quoted list

;; List operations
(car '(1 2 3))           ;; → 1 (first element)
(cdr '(1 2 3))           ;; → (2 3) (rest)
(cons 0 '(1 2 3))        ;; → (0 1 2 3)

;; List functions
(length '(1 2 3))        ;; → 3
(reverse '(1 2 3))       ;; → (3 2 1)
(map (lambda (x) (* x 2)) '(1 2 3))  ;; → (2 4 6)
```

### Objects as Maps
```reactor
;; Object creation
(def person (:name "Alice" :age 30))

;; Property access
person.name              ;; → "Alice"
person.age               ;; → 30

;; Dynamic property access
(get person :name)       ;; → "Alice"
(keys person)            ;; → (:name :age)
(values person)          ;; → ("Alice" 30)
```

## Modules and Namespaces

### Module Definition
```reactor
;; Module files define exports
;; math.x.reactor
(def pi 3.14159)
(def cos (lambda (x) ...))  ;; Implementation
;; (export pi cos) - if explicit exports needed
```

### Module Usage
```reactor
;; Import specific functions
(import math.x)
math.x.cos               ;; → cosine function
math.x.pi                ;; → 3.14159

;; Import namespace
(import math)            ;; If supported
math.x.cos               ;; → cosine function
math.y.sin               ;; → sine function
```

## Special Forms

### Quoting (`'`)
```reactor
;; Quote prevents evaluation
'(+ 1 2)                 ;; → (+ 1 2) as data
(+ 1 2)                  ;; → 3 as code

;; Quote objects
'(:name "Alice")         ;; → (:name "Alice") as data

;; Nested quoting
''foo                    ;; → (quote foo)
```

### Evaluation (`eval`)
```reactor
;; Evaluate data as code
(eval '(+ 1 2))          ;; → 3
(eval (list '+ 1 2))     ;; → 3

;; Dynamic code generation
(def op '+)
(eval (list op 1 2))     ;; → 3
```

## Operators

### Arithmetic
```reactor
(+ 1 2 3)                ;; → 6
(- 10 3)                 ;; → 7
(* 2 3 4)                ;; → 24
(/ 24 3)                 ;; → 8
(% 17 5)                 ;; → 2 (modulo)
```

### Comparison
```reactor
(= 1 1)                  ;; → true
(!= 1 2)                 ;; → true
(< 1 2 3)                ;; → true (1 < 2 < 3)
(<= 1 1 2)               ;; → true
(> 3 2 1)                ;; → true
(>= 3 3 2)               ;; → true
```

### String Operations
```reactor
(str "Hello" " " "World")  ;; → "Hello World"
(length "hello")          ;; → 5
(substring "hello" 1 3)   ;; → "el"
(concat "foo" "bar")      ;; → "foobar"
```

## Advanced Features

### Pattern Matching (if supported)
```reactor
;; Hypothetical pattern matching
(match value
  (:type "user" :name n)  (str "User: " n)
  (:type "admin")         "Admin"
  _                       "Unknown")
```

### Macros (if supported)
```reactor
;; Hypothetical macro definition
(defmacro when (condition &body)
  `(if ,condition (do ,@body)))

;; Usage
(when (> x 0)
  (print "Positive")
  (set result x))
```

### Error Handling
```reactor
;; Try/catch (if supported)
(try
  (/ 10 0)
  (catch DivisionByZero
    "Cannot divide by zero"))

;; Assertions
(assert (> x 0) "x must be positive")
```

## Grammar

### EBNF Grammar

```
program     ::= expr*

expr        ::= atom
              | list
              | prop_list
              | quoted_expr
              | symbol

atom        ::= number | string | symbol | boolean

list        ::= "(" expr* ")"

prop_list   ::= "(" (":" symbol expr)* ")"

quoted_expr ::= "'" expr

symbol      ::= letter (letter | digit | "-" | "_" | ".")*

number      ::= digit+ ["." digit+] [("e"|"E") ["+"|"-"] digit+]

string      ::= '"' char* '"'

boolean     ::= "true" | "false"
```

### Operator Precedence

Reactor is a Lisp-1 with no operator precedence:
- All operations are function calls
- Grouping is explicit with parentheses
- Left-to-right evaluation within expressions

### Evaluation Order

1. **Parse** source into AST
2. **Compile** AST to IR (resolves dotted symbols)
3. **Evaluate** IR in lexical environment
4. **Return** result or error

## Examples

### Hello World
```reactor
(print "Hello, World!")
```

### Factorial
```reactor
(def factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(factorial 5)  ;; → 120
```

### Object Processing
```reactor
(def users ((:name "Alice" :age 30)
            (:name "Bob" :age 25)
            (:name "Charlie" :age 35)))

(def get-names
  (lambda (users)
    (map (lambda (user) user.name) users)))

(get-names users)  ;; → ("Alice" "Bob" "Charlie")
```

### Configuration Management
```reactor
(def config (:debug false :port 8080))

;; Local configuration extension
(lambda (env)
  (def config.debug true)
  (def config.port 3000)
  (def config.database (:host "localhost" :port 5432))
  (start-server config))
```

This syntax reference covers all major Reactor language constructs and provides a foundation for writing Reactor programs.
