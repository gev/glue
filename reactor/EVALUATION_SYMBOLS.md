# Symbol Evaluation

Symbol evaluation resolves simple identifiers to their bound values in the current environment.

## Symbol Resolution

**Input IR:** `Symbol name`
**Process:** Look up `name` in environment frames from top to bottom
**Output:** Bound value or UnboundVariable error

### Lookup Algorithm
1. Start with current environment (top frame)
2. Search each frame from top to bottom
3. Return first matching binding
4. Raise UnboundVariable if not found in any frame

### Environment Search Order
- **Top Frame:** Most recently pushed frame (current scope)
- **Bottom Frame:** First pushed frame (contains initial bindings)
- **Direction:** Top → Bottom (most local to most global)

## Symbol Examples

```reactor
;; Variable lookup
x           ;; Find 'x' in environment

;; Function names
println     ;; Find 'println' function

;; Module names
math        ;; Find 'math' module

;; Constants
pi          ;; Find 'pi' constant
```

## Error Conditions

### UnboundVariable
**Cause:** Symbol name not found in any environment frame
**Context:** Symbol name, current environment state
**Example:** `undefined_var` when `undefined_var` is not defined

## Performance Characteristics

- **Lookup Time:** O(depth) where depth is frame stack height
- **Typical Case:** Fast for local variables (shallow stack)
- **Worst Case:** Deep recursion with many frames
- **Caching:** No caching - resolved on each access

## Implementation Details

### Frame Structure
```haskell
type Frame = Map Text IR
type Environment = [Frame]  -- Stack of frames
```

### Lookup Function
```haskell
lookupVar :: Text -> Environment -> Maybe IR
lookupVar name [] = Nothing
lookupVar name (frame:rest) =
    case Map.lookup name frame of
        Just val -> Just val
        Nothing -> lookupVar name rest
```

### Symbol Evaluation
```haskell
evalSymbol :: Text -> Eval (Maybe IR)
evalSymbol name = do
    env <- getEnv
    case lookupVar name env of
        Right val -> pure (Just val)
        Left err -> throwError err
```

## Scope Resolution

### Local Scope
Variables defined in current function/block:
```reactor
(def local 42)  ;; Available in current scope
local           ;; → 42
```

### Function Parameters
Parameters bound at function entry:
```reactor
(lambda (param)
  param)        ;; 'param' available in function body
```

### Global Scope
Builtins and user-defined globals:
```reactor
(+ 1 2)         ;; '+' is a builtin
```

## See Also

- [Dotted Symbol Evaluation](EVALUATION_DOTTED_SYMBOLS.md) - Property access with dot notation
- [Environment](ENVIRONMENT.md) - Environment structure and frame management
- [Evaluation](EVALUATION.md) - Main evaluation overview
