# Symbol Evaluation

Symbol evaluation resolves identifiers to their bound values in the current environment.

## Simple Symbol Evaluation

**Input IR:** `Symbol name`
**Process:** Look up `name` in environment frames from top to bottom
**Output:** Bound value or UnboundVariable error

### Lookup Process
1. Start with current environment (top frame)
2. Search each frame from top to bottom
3. Return first matching binding
4. Raise UnboundVariable if not found

### Environment Frames
- **Local Frames:** Function parameters, local variables
- **Global Frame:** Builtin functions, user-defined globals
- **Search Order:** Local → Function → Global

## Dotted Symbol Evaluation

**Input IR:** `DottedSymbol [part1, part2, ...]`
**Process:** Traverse object/module properties hierarchically
**Output:** Final property value or error

### Traversal Process
1. Evaluate first part as symbol
2. For each subsequent part:
   - Access property on current object
   - Update current object to property value
3. Return final value

### Property Access
- **Objects:** Access by key in map
- **Modules:** Access exported symbols
- **Errors:** PropertyNotFound, NotAnObject

## Symbol Resolution Examples

```reactor
;; Simple symbol
x  ;; Lookup 'x' in environment

;; Dotted access
obj.field        ;; Access 'field' property of obj
module.func      ;; Access 'func' export from module
data.user.name   ;; Deep property access
```

## Error Conditions

### UnboundVariable
**Cause:** Symbol name not found in any environment frame
**Context:** Symbol name, current environment state

### PropertyNotFound
**Cause:** Object/module doesn't have requested property
**Context:** Property name, object type

### NotAnObject
**Cause:** Attempted property access on non-object value
**Context:** Value type, property name

## Performance Characteristics

- **Simple Lookup:** O(depth) where depth is frame stack height
- **Dotted Access:** O(depth + path_length)
- **Caching:** No caching - resolved on each access
- **Sharing:** Environment frames shared between calls

## Implementation Notes

### Frame Structure
```haskell
type Frame = Map Text IR
type Environment = [Frame]  -- Stack
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

### Dotted Resolution
```haskell
resolveDotted :: [Text] -> Environment -> Either Error IR
resolveDotted [] _ = Left EmptyDottedPath
resolveDotted (first:rest) env = do
    obj <- lookupVar first env
    resolvePath obj rest
```

## See Also

- [Environment](ENVIRONMENT.md) - Environment structure and frame management
- [Evaluation](EVALUATION.md) - Main evaluation overview
