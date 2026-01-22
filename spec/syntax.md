# Glue Language Syntax Specification

## Overview

Glue employs a Lisp-inspired syntax with extensions for property objects, functional programming constructs, and module systems. This specification defines the complete syntax and grammar for Glue programs.

## Lexical Elements

### Atoms

Atoms represent primitive values that evaluate to themselves.

#### Numbers
```clojure
42          ; integer
3.14159     ; float
-273.15     ; negative float
```

#### Strings
```clojure
"hello world"
"quote: \" and backslash: \\"
```

#### Symbols
Symbols serve as identifiers.
```clojure
my-variable
function-name
x
y
math.pi
```

### Comments
Comments are ignored during evaluation.
```clojure
;; single line comment

;; multi-line comments
;; are multiple single-line comments

(+ 1 2)  ;; inline comment
```

## Expressions

### Lists

Lists are ordered sequences enclosed in parentheses. Evaluation depends on the first element:

#### Function Calls
Lists beginning with a symbol are evaluated as function applications.
```clojure
(+ 1 2 3)              ; addition
(* (+ 1 2) (+ 3 4))    ; nested calls
(now)                   ; no arguments - symbol evaluated
(negate -5)             ; single argument
```

#### Data Lists
Lists not beginning with a symbol are evaluated as data structures.
```clojure
(1 2 3)                 ; literal numbers
(("a" "b") ("c" "d"))   ; nested lists
```



### Property Objects

Property objects are key-value collections.

#### Creation
```clojure
(:)                     ; empty object
(:name "Alice" :age 30) ; simple properties
(:user (:name "Bob" :age 25)
 :config (:theme "dark" :lang "en")) ; nested
(:result (* 2 21) :timestamp (now)) ; computed values
```

#### Access
```clojure
(:name "Alice").name           ; direct access
(:user (:name "Bob")).user.name  ; nested access
user.name                      ; dotted notation
user.address.city              ; deep access
```

#### Update
```clojure
(set user.age 31)
(set user.email "alice@example.com")
(set config.database.host "localhost")
```

### Special Forms

Special forms have evaluation rules different from regular function calls.

#### Definition
```clojure
(def x 42)
(def greeting "Hello")
(def config (:debug true :port 8080))
(def config.timeout 5000)

;; Function definition sugar (Scheme-style)
(def (square x) (* x x))
(def (add x y) (+ x y))
(def (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

#### Mutation
```clojure
(set x 100)
(set user.name "Bob")
(set config.database.host "localhost")
```

#### Import
```clojure
(import math.x)
(import math)
```

#### Lambda
```clojure
(lambda (x) (* x x))
(lambda (a b) (+ a b))
(lambda () "hello")
```

#### Conditional
```clojure
(if (> x 0) "positive" "non-positive")
(if (even? x) "even" "odd")
```



## Operators

### Arithmetic
```clojure
(+ 1 2 3)    ; addition
(- 10 3)     ; subtraction
(* 2 3 4)    ; multiplication
(/ 24 3)     ; division
(% 17 5)     ; modulo
```

### Comparison
```clojure
(== 1 1)     ; equality
(!= 1 2)     ; inequality
(< 1 2 3)    ; less than
(<= 1 1 2)   ; less than or equal
(> 3 2 1)    ; greater than
(>= 3 3 2)   ; greater than or equal
```

### Logical
```clojure
(& (> x 0) (< x 100))
(| (= x 0) (= x 1))
(! (= x 0))
```

### String
```clojure
(str "Hello" " " "World")
(length "hello")
(substring "hello" 1 3)
(concat "foo" "bar")
```

## Data Structures

### Lists
```clojure
(car (1 2 3))           ; first element
(cdr (1 2 3))           ; rest
(cons 0 (1 2 3))        ; prepend
(length (1 2 3))        ; length
(reverse (1 2 3))       ; reverse
```

### Objects as Maps
```clojure
(def person (:name "Alice" :age 30))
person.name
(keys person)
(values person)
```

## Modules

### Module Definition
Modules define namespaces and exports.
```clojure
(module math.x.glue
    (export pi cos)
    (def pi 3.14159)
    (def cos (lambda (x) ...)))
```

### Module Usage
```clojure
(import math.x)
(cos pi)        ; exports available directly
```

## Grammar

### EBNF Specification

```ebnf
program         ::= expr
expr            ::= atom | list | prop_list
atom            ::= number | string | symbol
list            ::= "(" expr* ")"
prop_list       ::= "(" (":" symbol expr)* ")"
symbol          ::= (letter | special_char) (letter | digit | special_char | ":")*
number          ::= ["+" | "-"] digit+ ["." digit+] [("e"|"E") ["+"|"-"] digit+]
string          ::= '"' char* '"'
special_char    ::= arithmetic | comparison | logical | separators
arithmetic      := "+" | "-" | "*" | "/" | "%"
comparison      := "=" | "<" | ">"
logical         := "&" | "|" | "!"
separators      := "?" | "\" | "$" | "@" | "#" | "_" | "." | "'"
```

### Evaluation Semantics

Glue follows Lisp-1 evaluation rules:
- Symbols resolve in the current lexical environment
- Lists are evaluated based on their first element: as function applications if starting with a symbol, as data structures otherwise
- Property objects are evaluated with computed values
- Special forms have custom evaluation rules

## Related documents

- [AST Specification](ast.md) - Abstract syntax tree structure
- [Parsing Specification](parsing-to-ast.md) - Converting syntax to AST
