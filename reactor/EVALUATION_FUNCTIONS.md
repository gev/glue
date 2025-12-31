# Function Application

Function application evaluates callable constructs with their arguments.

## Native Functions

Native functions are host language implementations.

**Input:** `List [Native func, arg1, arg2, ...]`
**Process:** Evaluate arguments, apply native function

**Types:**
- **Functions:** Return evaluated results
- **Commands:** Perform side effects, return nothing
- **Special Forms:** Handle special evaluation rules

## Closures

Closures are user-defined functions with captured environment.

**Input:** `List [Closure, arg1, arg2, ...]`
**Process:** Evaluate arguments, bind to parameters, evaluate body in new environment

**Parameter Binding:** Arguments bound to closure parameters in new environment frame

## Symbol Lookup

Symbols resolve to callables through environment lookup.

**Input:** `List [Symbol name, arg1, arg2, ...]`
**Process:** Look up symbol, apply result to arguments if callable

## Argument Evaluation

All arguments are evaluated before function application (eager evaluation).

## Error Conditions

- **WrongNumberOfArguments:** Parameter/argument count mismatch
- **NotCallableObject:** Attempted to call non-callable value
- **UnboundVariable:** Symbol not found in environment
