# Evaluation: Symbol Resolution

## Overview

Symbol evaluation resolves identifiers to their bound values in the current environment. Reactor supports both simple symbols and dotted symbols for hierarchical access.

## Simple Symbol Evaluation

**Input IR:** `Symbol name`
**Process:** Look up `name` in environment frames
**Output:** Bound value or error

### Lookup Process
1. Start with current environment (innermost frame)
2. Search each frame from top to bottom
3. Return first matching binding
4. If no binding found, throw `UnboundVariable` error

### Examples
- `Symbol "x"` → looks up variable `x`
- `Symbol "+"` → looks up addition function
- `Symbol "my-function"` → looks up user-defined function

## Dotted Symbol Evaluation

**Input IR:** `DottedSymbol [part1, part2, ...]`
**Process:** Resolve hierarchical property access
**Output:** Nested property value or error

### Resolution Algorithm
1. Try full dotted path as simple symbol first
2. If not found, try progressively shorter prefixes
3. For longest matching prefix, access remaining parts as properties
4. Properties can be accessed on objects or modules

### Examples
- `DottedSymbol ["math", "pi"]` → find "math", access property "pi"
- `DottedSymbol ["obj", "field", "sub"]` → find "obj", access "field", then "sub"
- `DottedSymbol ["module", "func"]` → find "module", access exported "func"

## Property Access

### Object Properties
When accessing properties on objects:
- Check if target is an `Object` type
- Look up property name in object's property map
- Return property value or `PropertyNotFound` error

### Module Properties
When accessing properties on modules:
- Check if target is a `Module` type
- Look up export name in module's export map
- Return exported value or `PropertyNotFound` error

### Module Access via Dotted Symbols
Dotted symbols can access module exports:
- `math.pi` → access "pi" export from "math" module
- `utils.string.trim` → access "trim" from "string" submodule of "utils"
- Module access happens through the module registry and import cache

## Error Conditions

### UnboundVariable
**Cause:** Symbol name not found in any environment frame
**Context:** Includes symbol name and current call stack

### PropertyNotFound
**Cause:** Property name doesn't exist on target object/module
**Context:** Includes property name and target type

### NotAnObject
**Cause:** Attempted property access on non-object value
**Context:** Includes actual type of target value

## Environment Scoping

### Lexical Scoping
Symbols are resolved using lexical scoping rules:
- Local variables shadow global ones
- Function parameters are accessible in function body
- Closures capture their definition environment

### Frame Structure
Environment is a stack of frames:
- Top frame: current function locals
- Middle frames: enclosing scopes
- Bottom frame: global definitions

## Special Cases

### Reserved Symbols
Certain symbols may be bound to built-in functions or special forms by the runtime environment.

### Dynamic Lookup
Symbol resolution happens at evaluation time, enabling dynamic binding and late resolution.
