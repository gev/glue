# Closure Evaluation

Closure evaluation executes user-defined functions with their captured environment and parameters.

## Closure Structure

**Input IR:** `Closure [param1, param2, ...] body capturedEnv`
**Process:** Bind parameters to arguments, evaluate body in extended environment
**Output:** Body evaluation result

### Components
- **Parameters:** `[Text]` - parameter names to bind
- **Body:** `IR` - function body expression
- **Environment:** `Env` - captured lexical environment

## Evaluation Process

### Parameter Binding
1. **Receive arguments:** `[arg1, arg2, ...]` (unevaluated)
2. **Create frame:** New environment frame with parameter bindings
3. **Extend environment:** Add frame to captured closure environment
4. **Evaluate body:** Execute body in extended environment

### Environment Extension
```
Captured Environment
    ↓
[New Frame: param1=arg1, param2=arg2, ...]
    ↓
Extended Environment (used for body evaluation)
```

## Closure Examples

```closure
;; Define closure
(def add (lambda (x y) (+ x y)))

;; Call closure
(add 3 4)  ;; → 7

;; Closure with captured variables
(def make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(def add5 (make-adder 5))
(add5 3)  ;; → 8 (captures n=5)
```

## Parameter Handling

### Argument Count Validation
- Parameter count must match argument count
- Error: `WrongNumberOfArguments` if mismatch

### Argument Evaluation
- Arguments received unevaluated
- Evaluated as needed within closure body
- Allows lazy evaluation patterns

## Environment Management

### Lexical Scoping
- Closures capture definition-time environment
- Variables resolved in lexical scope
- Supports nested function definitions

### Frame Lifecycle
- New frame created for each call
- Frame discarded after evaluation
- No pollution of global environment

## Error Conditions

### WrongNumberOfArguments
**Cause:** Parameter/argument count mismatch
**Context:** Expected parameters vs received arguments

### Errors in Body
**Cause:** Runtime errors during body evaluation
**Context:** Error location within closure body

## Performance Characteristics

### Call Overhead
- Frame creation and environment extension
- Parameter binding cost
- Environment lookup in captured scope

### Memory Usage
- Captured environment shared between calls
- New frame allocated per call
- Automatic cleanup after evaluation

## Implementation Details

### Closure Creation
```haskell
-- Lambda creates closure
(lambda (params...) body) → Closure params body currentEnv
```

### Closure Application
```haskell
applyClosure :: [Text] -> IR -> Env -> [IR] -> Eval (Maybe IR)
applyClosure params body capturedEnv args = do
    -- Validate argument count
    -- Create parameter bindings
    -- Extend environment
    -- Evaluate body
```

## Common Patterns

### Higher-Order Functions
```closure
(def map (lambda (f list)
  (if (empty? list)
      ()
      (cons (f (first list))
            (map f (rest list))))))

(map (lambda (x) (* x 2)) (1 2 3))  ;; → (2 4 6)
```

### Function Factories
```closure
(def make-counter
  (lambda (start)
    (lambda ()
      (set start (+ start 1))
      start)))

(def counter (make-counter 0))
(counter)  ;; → 1
(counter)  ;; → 2
```

## See Also

- [Function Application](EVALUATION_FUNCTIONS.md) - General function calling
- [Environment](ENVIRONMENT.md) - Variable scoping and frames
- [Evaluation](EVALUATION.md) - Main evaluation overview
