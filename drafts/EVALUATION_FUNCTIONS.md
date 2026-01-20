# Function Application & Currying

Function application evaluates lists where the first element is callable. Glue implements **universal currying** - all functions follow a single-argument contract with automatic partial application. Glue supports multiple types of callable values: native functions, closures, and dynamically resolved symbols.

## Function Call Detection

**Input IR:** `List [first, arg1, arg2, ...]`
**Process:** Check the type of `first`:
- If `first` is `Native` or `Closure`: Direct function application
- If `first` is `Symbol`: Evaluate symbol, check if result is callable
- Otherwise: Treat as data list
**Output:** Function result or data list

### Callable Types
- `Native` - Host language functions and special forms (first element IS the callable)
- `Closure` - User-defined functions with captured environment (first element IS the callable)
- Functions resolved through symbol lookup (first element evaluates to callable)

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

## Universal Currying & Partial Application

Glue implements **universal currying** - all functions follow a single-argument contract with automatic partial application. This enables functional programming patterns where functions can be partially applied naturally.

### Single-Argument Contract
- **All functions take exactly 1 argument**
- **Multi-argument functions** are syntactic sugar for nested single-argument functions
- **Partial application** returns a new function expecting the remaining arguments

### Currying Examples
```glue
;; Multi-arg function (syntactic sugar)
(lambda (a b c) (+ a b c))

;; Desugars to nested functions
(lambda (a)
  (lambda (b)
    (lambda (c)
      (+ a b c))))

;; Partial application works naturally
(def add (lambda (a b) (+ a b)))
(def add5 (add 5))        ;; Returns function expecting 1 more arg
(add5 3)                  ;; → 8
```

### Native Function Currying
- **Arithmetic operators** support currying: `(+ 5)` returns function that adds 5
- **All built-in functions** follow single-argument contract
- **Automatic partial application** for any argument count

### Closure Currying
- **User-defined functions** support currying through partial application
- **Parameter binding** creates new closures with remaining parameters
- **Environment capture** preserves lexical scope

### Currying Benefits
- **Functional composition**: Easy function combination
- **Point-free style**: Omit arguments in pipelines
- **Reusability**: Create specialized functions from general ones
- **Type safety**: Gradual argument application

## Argument Evaluation

### Eager Evaluation
All function arguments are evaluated before function application:
1. Each argument IR node is evaluated
2. Results passed to function
3. Functions receive fully evaluated arguments

### Evaluation Order
Arguments evaluated left-to-right, but order not guaranteed to be strict.

## Error Conditions

### wrongNumberOfArguments
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
