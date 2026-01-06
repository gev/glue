# List Evaluation

List evaluation analyzes the input list structure and transforms it into a callable IR if possible, otherwise evaluates list elements as data.

## Single Symbol Lists

**Input IR:** `List [Symbol name]`
**Process:** Look up `name` in environment. If callable, apply with no arguments. If not callable, return the looked-up value.
**Output:** Function result or symbol value

### Callable Check
- If symbol resolves to Native/Closure: Apply as zero-argument function
- If symbol resolves to other value: Return that value directly
- If symbol not found: UnboundVariable error

## Symbol with Arguments

**Input IR:** `List [Symbol name, arg1, arg2, ...]`
**Process:**
1. Look up `name` in environment → resolves to `Closure` or `Native`
2. Apply the resolved `Closure` or `Native` to unevaluated `arg1`, `arg2`, ...`
**Output:** Function application result

### Callable Resolution
- Symbol first resolves to callable value (`Closure` or `Native`)
- Then callable gets applied with arguments
- For `Closure` evaluation details, see [Closure Evaluation](EVALUATION_CLOSURES.md)
- For `Native` evaluation details, see [Native Evaluation](EVALUATION_NATIVES.md)

### Argument Handling
- Arguments passed unevaluated to callable
- Callable receives raw IR nodes
- Callable decides whether to evaluate arguments

## General Lists

**Input IR:** `List [item1, item2, ...]`
**Process:** Evaluate all items. If first evaluated item is callable, apply it to remaining evaluated items. Otherwise return list of all evaluated items.
**Output:** Function result or evaluated data list

### Evaluation Order
1. Evaluate `item1`, `item2`, ... in order
2. Check if `evaluated_item1` is callable
3. If callable: Apply to `[evaluated_item2, ...]`
4. If not callable: Return `List [evaluated_item1, evaluated_item2, ...]`

## Function vs Data Distinction

Lists serve dual purpose based on evaluation:

### Function Calls
- First element evaluates to callable (Native, Closure, or function)
- Arguments processed according to function type
- Result is function return value

### Data Lists
- First element evaluates to non-callable value
- All elements evaluated as data
- Result is new list with evaluated elements

## List Evaluation Examples

```closure
;; Function call - single symbol
(println)  ;; Call println with no args

;; Function call - symbol with args
(+ 1 2 3)  ;; Call + with args 1, 2, 3

;; Data list
(1 2 3)    ;; List of numbers

;; Mixed evaluation
(list (+ 1 2) "hello")  ;; → (3 "hello")
```

## Error Conditions

### UnboundVariable
**Cause:** Symbol in function position not found
**Context:** Symbol name, list position

### NotCallableObject
**Cause:** First evaluated item is not callable in general lists
**Context:** Value type, evaluation result

### wrongNumberOfArguments
**Cause:** Function called with wrong argument count
**Context:** Expected vs actual count

## Call Stack Management

### Context Tracking
- Function name pushed to call stack before evaluation
- Special context "<call>" used for general function calls
- Popped after evaluation completes

### Error Context
Call stack provides:
- Location of function calls
- Nested call information
- Debugging context for errors

## Performance Considerations

### Analysis Cost
- Symbol-only lists: Single environment lookup
- General lists: Full evaluation of all elements
- Short-circuiting: Stops at first callable check

### Memory Usage
- Argument lists: Passed by reference until evaluation
- Result construction: New list created for data lists
- Environment frames: Added for function calls

## Implementation Details

### Pattern Matching
```haskell
evalList :: [IR] -> Eval (Maybe IR)
evalList [IR.Symbol name] = do
    pushContext name
    -- Symbol-only logic
    popContext

evalList (IR.Symbol name : rawArgs) = do
    pushContext name
    -- Symbol with args logic
    popContext

evalList xs = do
    -- General list logic
```

### Callable Detection
```haskell
isCallable :: IR -> Bool
isCallable (IR.Native _) = True
isCallable (IR.Closure{}) = True
isCallable _ = False
```

## See Also

- [Function Application](EVALUATION_FUNCTIONS.md) - Detailed function calling semantics
- [Data Structures](EVALUATION_DATA.md) - Data list evaluation
- [Evaluation](EVALUATION.md) - Main evaluation overview
