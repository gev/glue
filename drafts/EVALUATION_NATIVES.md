# Native Function Evaluation

Native function evaluation executes host language functions integrated into the Glue runtime.

## Native Structure

**Input IR:** `NativeFunc impl` or `Special impl`
**Process:** Execute host language function with provided arguments
**Output:** Function result or side effect

### Native Types
- **NativeFunc:** `([IR] -> m IR)` - Functions that return evaluated results
- **Special:** `([IR] -> m IR)` - Special forms with custom evaluation rules
- **NativeValue:** `HostValue` - Host objects that don't need evaluation

## Evaluation Process

### Function Application
1. **Receive arguments:** `[arg1, arg2, ...]` (evaluated or raw)
2. **Execute native code:** Call host language implementation
3. **Return result:** Function result or side effect confirmation

### Argument Handling
- **NativeFunc:** Arguments pre-evaluated by caller
- **Special:** Arguments passed raw, special evaluation rules apply
- **NativeValue:** No evaluation needed (already host values)

## Native Function Examples

```closure
;; Arithmetic function
(+ 1 2 3)  ;; Native Func: adds numbers → 6

;; I/O command
(println "hello")  ;; Native Cmd: prints to console, returns nothing

;; Special form
(if condition true-branch false-branch)  ;; Native Special: conditional evaluation
```

## Host Object Integration Examples

Host objects integrate Haskell data types into Glue runtime with property access and mutation.

### Haskell Data Types
```haskell
data Person = Person {name :: Text, age :: Int}
data Address = Address {street :: Text, city :: Text}
data PersonWithAddress = Person {name :: Text, age :: Int, address :: Maybe Address}
```

### Object Creation and Property Access
```glue
;; Create host objects using constructor functions
(def bob (person :name "Bob" :age 25))
(def addr (address :street "123 Main St" :city "Springfield"))

;; Access properties
bob.name        ;; → "Bob"
bob.age         ;; → 25
addr.street     ;; → "123 Main St"
addr.city       ;; → "Springfield"
```

### Property Modification
```glue
;; Modify existing properties
(set bob.age 26)        ;; → 26
(set bob.name "Robert") ;; → "Robert"
(set addr.city "Boston") ;; → "Boston"

;; Verify changes persist
bob.age     ;; → 26
bob.name    ;; → "Robert"
addr.city   ;; → "Boston"
```

### Nested Object Relationships
```glue
;; Create objects with nested relationships
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))

;; Access nested properties
bob.address.city        ;; → "Springfield"
bob.address.street      ;; → "123 Main St"

;; Modify nested properties
(set bob.address.city "Boston")      ;; → "Boston"
(set bob.address.street "456 Oak Ave") ;; → "456 Oak Ave"

;; Verify nested changes
bob.address.city        ;; → "Boston"
bob.address.street      ;; → "456 Oak Ave"
```

### Complex Object Manipulation
```glue
;; Multiple sequential operations
(def addr (address :street "123 Main St" :city "Springfield"))
(def bob (person :name "Bob" :age 25 :address addr))

(set bob.age 26)
(set bob.name "Robert")
(set bob.address.city "Boston")
(set bob.address.street "456 Oak Ave")

bob.name                ;; → "Robert"
bob.age                 ;; → 26
bob.address.city        ;; → "Boston"
bob.address.street      ;; → "456 Oak Ave"
```

## Function Categories

### NativeFunc
- **Purpose:** Compute and return values
- **Examples:** `+`, `-`, `*`, `/`, `str`, `list`
- **Return:** Always returns an `IR` value
- **Evaluation:** Pure computation

### Special
- **Purpose:** Control evaluation flow
- **Examples:** `if`, `lambda`, `def`, `set`
- **Return:** May return value or control structure
- **Evaluation:** Non-standard argument evaluation

### NativeValue
- **Purpose:** Store host language objects
- **Examples:** Foreign objects, complex data structures
- **Return:** Self (no evaluation needed)
- **Evaluation:** Identity (returns unchanged)

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
registerNative :: Text -> IR m -> Env m -> Env m

-- Example: register addition
registerNative "+" (NativeFunc addImpl) env

-- Example: register special form
registerNative "if" (Special ifImpl) env

-- Example: register host value
registerNative "pi" (NativeValue (hostValue (3.14159 :: Double))) env
```

### Function Implementation
```haskell
-- Example native function
addImpl :: [IR] -> Eval IR
addImpl args = do
    numbers <- mapM extractNumber args
    pure $ Number $ sum numbers
```

### Special Form Implementation
```haskell
-- Example special form
ifImpl :: [IR] -> Eval IR
ifImpl [condition, trueBranch, falseBranch] = do
    condResult <- eval condition
    case condResult of
        Bool True -> eval trueBranch
        Bool False -> eval falseBranch
        _ -> throwError typeError
ifImpl _ = throwError wrongNumberOfArguments
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
