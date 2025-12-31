# Function Application

## Native Functions

**Input:** `List [Native func, arg1, arg2, ...]`
**Process:** Evaluate `arg1`, `arg2`, ... and apply `Native func` to results

## Closures

**Input:** `List [Closure, arg1, arg2, ...]`
**Process:** Evaluate `arg1`, `arg2`, ... and apply `Closure` to results

## Symbol Lookup

**Input:** `List [Symbol name, arg1, arg2, ...]`
**Process:** Look up `name` in environment and apply result to `arg1`, `arg2`, ...`
