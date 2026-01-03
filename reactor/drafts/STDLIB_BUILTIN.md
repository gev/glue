# Builtin Module

The builtin module provides essential special forms and core functionality for Reactor programs.

## Overview

The builtin module includes fundamental operations that form the basis of Reactor programming:

- **def** - Variable definition
- **lambda** (\\) - Function creation
- **set** - Variable mutation
- **import** - Module loading

## def

Defines a new variable in the current environment.

**Syntax:**
```
(def symbol value)
```

**Parameters:**
- `symbol` - The name to bind the value to
- `value` - The value to bind (evaluated)

**Examples:**
```
(def x 42)
(def greeting "hello")
```

## lambda (\\)

Creates an anonymous function.

**Syntax:**
```
(lambda (param1 param2 ...) body)
(\\ (param1 param2 ...) body)
```

**Parameters:**
- `param1 param2 ...` - Function parameters
- `body` - Function body expression

**Examples:**
```
(lambda (x) (* x 2))
(\\ (a b) (+ a b))
```

## set

Updates the value of an existing variable.

**Syntax:**
```
(set symbol value)
```

**Parameters:**
- `symbol` - The variable to update
- `value` - The new value (evaluated)

**Examples:**
```
(def x 1)
(set x 2)  ; x is now 2
```

## import

Loads and imports a module, making its exports available.

**Syntax:**
```
(import module-name)
```

**Parameters:**
- `module-name` - The name of the module to import

**Examples:**
```
(import "math")
(import "my-module")
```

## See Also

- [Standard Library Introduction](STDLIB_INTRO.md)
- [Bool Module](STDLIB_BOOL.md)
