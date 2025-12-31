# Evaluation: Function Application

## Overview

Function application evaluates lists where the first element is callable. Reactor supports multiple types of callable values: native functions, closures, and dynamically resolved symbols.

## Function Call Detection

**Input IR:** `List [first, arg1, arg2, ...]`
**Process:** Check if `first` evaluates to a callable value
**Output:** Function result or data list

### Callable Types
- `Native` - Host language functions and special forms
- `Closure` - User-defined functions with captured environment
- Functions resolved through symbol lookup

## Native Function Application

**Input:** `List [Native func, arg1, arg2, ...]`
**Process:**
1. Evaluate all arguments
2. Apply native function to evaluated arguments
3. Return function result

### Native Function Types

#### Functions (`Func`)
- **Signature:** `([IR] -> Eval IR)`
- **Result:** Returns evaluated result
- **Example:** Arithmetic operations, string manipulation

#### Commands (`Cmd`)
- **Signature:** `([IR] -> Eval ())`
- **Result:** No return value (side effects only)
- **Example:** I/O operations, state mutations

#### Special Forms (`Special`)
- **Signature:** `([IR] -> Eval (Maybe IR))`
- **Result:** May return value or indicate special handling
- **Example:** Control flow, macro-like operations

## Closure Application

**Input:** `List [Closure params body env, arg1, arg2, ...]`
**Process:**
1. Evaluate all arguments
2. Create new environment frame with parameter bindings
3. Evaluate closure body in new environment
4. Return body evaluation result

### Parameter Binding
- Parameters bound to evaluated arguments
- Argument count must match parameter count
- New frame added to captured environment

### Environment Handling
- Closure captures definition-time environment
- New frame contains parameter bindings
- Body evaluates in extended environment

## Symbol Function Application

**Input:** `List [Symbol name, arg1, arg2, ...]`
**Process:**
1. Look up symbol in environment
2. If result is callable, apply it to arguments
3. Otherwise, return as data list

### Dynamic Resolution
- Symbol resolved at call time
- Enables dynamic function binding
- Supports higher-order programming

## Argument Evaluation

### Eager Evaluation
All function arguments are evaluated before function application:
1. Each argument IR node is evaluated
2. Results passed to function
3. Functions receive fully evaluated arguments

### Evaluation Order
Arguments evaluated left-to-right, but order not guaranteed to be strict.

## Error Conditions

### WrongNumberOfArguments
**Cause:** Argument count doesn't match function parameter count
**Context:** Expected vs actual argument counts

### NotCallableObject
**Cause:** Attempted to call non-callable value
**Context:** Type of attempted callable

### UnboundVariable
**Cause:** Symbol function name not found
**Context:** Symbol name and lookup failure

## Call Stack Management

### Context Tracking
- Function name pushed to call stack before evaluation
- Popped after evaluation completes
- Enables error location reporting

### Error Context
Call stack includes:
- Function names in call chain
- Location of each call
- Helps debug complex function calls

## Special Cases

### Nullary Functions
Functions with no arguments: `List [func]`
- No arguments to evaluate
- Direct function application

### Variadic Functions
Some functions accept variable argument counts:
- Argument evaluation still required
- Function handles argument count internally

### Recursive Calls
Functions can call themselves:
- Each call gets new environment frame
- Proper stack management prevents infinite recursion detection
