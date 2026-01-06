# Native Function Evaluation

Native function evaluation executes host language functions integrated into the Glue runtime.

## Native Structure

**Input IR:** `Native nativeImpl`
**Process:** Execute host language function with provided arguments
**Output:** Function result or side effect

### Native Types
- **Functions:** `Func ([IR] -> m IR)` - Return evaluated result
- **Commands:** `Cmd ([IR] -> m ())` - Perform side effects only
- **Special Forms:** `Special ([IR] -> m (Maybe IR))` - Special evaluation rules

## Evaluation Process

### Function Application
1. **Receive arguments:** `[arg1, arg2, ...]` (evaluated or raw)
2. **Execute native code:** Call host language implementation
3. **Return result:** Function result or side effect confirmation

### Argument Handling
- **Functions:** Arguments pre-evaluated by caller
- **Commands:** Arguments pre-evaluated by caller
- **Special Forms:** Arguments passed raw, special evaluation rules apply

## Native Function Examples

```closure
;; Arithmetic function
(+ 1 2 3)  ;; Native Func: adds numbers → 6

;; I/O command
(println "hello")  ;; Native Cmd: prints to console, returns nothing

;; Special form
(if condition true-branch false-branch)  ;; Native Special: conditional evaluation
```

## Function Categories

### Functions (`Func`)
- **Purpose:** Compute and return values
- **Examples:** `+`, `-`, `*`, `/`, `str`, `list`
- **Return:** Always returns an `IR` value
- **Evaluation:** Pure computation

### Commands (`Cmd`)
- **Purpose:** Perform side effects
- **Examples:** `println`, `read-file`, `write-file`
- **Return:** Nothing (void)
- **Evaluation:** May modify external state

### Special Forms (`Special`)
- **Purpose:** Control evaluation flow
- **Examples:** `if`, `lambda`, `def`, `set`
- **Return:** May return value or control structure
- **Evaluation:** Non-standard argument evaluation

## Error Conditions

### wrongNumberOfArguments
**Cause:** Native function called with wrong argument count
**Context:** Function name, expected vs actual count

### wrongArgumentType
**Cause:** Arguments have incorrect types for native function
**Context:** Function name, expected types, actual types

### Native Implementation Errors
**Cause:** Errors in host language native code
**Context:** Function name, specific error details

## Performance Characteristics

### Call Overhead
- Host language function call overhead
- Argument marshalling between languages
- Result conversion back to IR

### Optimization Potential
- Direct compilation to host language calls
- Caching of frequently used natives
- Inline expansion for simple operations

## Implementation Details

### Native Registration
```haskell
-- Register native function
registerNative :: Text -> Native m -> Env m -> Env m

-- Example: register addition
registerNative "+" (Func addImpl) env
```

### Function Implementation
```haskell
-- Example native function
addImpl :: [IR] -> Eval IR
addImpl args = do
    numbers <- mapM extractNumber args
    pure $ Number $ sum numbers
```

### Command Implementation
```haskell
-- Example native command
printlnImpl :: [IR] -> Eval ()
printlnImpl [IR.String text] = liftIO $ putStrLn text
printlnImpl _ = throwError wrongArgumentType
```

## Host Language Integration

### Type Marshalling
- Convert Glue `IR` values to host language types
- Execute host language logic
- Convert results back to `IR` values

### Error Propagation
- Catch host language exceptions
- Convert to Glue error types
- Maintain error context and call stack

## Common Native Functions

### Arithmetic
- `+`, `-`, `*`, `/` - Basic math operations
- `mod`, `pow` - Advanced math
- `min`, `max` - Comparisons

### String Operations
- `str` - String construction
- `substr` - Substring extraction
- `concat` - String concatenation

### List Operations
- `list` - List construction
- `first`, `rest` - List access
- `cons`, `append` - List modification

### I/O Operations
- `println` - Console output
- `read-file`, `write-file` - File operations
- `http-get`, `http-post` - Network operations

## Special Forms

### Control Flow
- `if` - Conditional evaluation
- `while` - Looping construct
- `try/catch` - Exception handling

### Definition
- `def` - Variable definition
- `lambda` - Function creation
- `set` - Variable assignment

### Evaluation Control
- `eval` - Force evaluation
- `macro` - Code transformation

## See Also

- [Function Application](EVALUATION_FUNCTIONS.md) - General function calling
- [Evaluation](EVALUATION.md) - Main evaluation overview
- [Standard Library](STDLIB_INTRO.md) - Available native functions
