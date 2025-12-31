# Dotted Symbol Evaluation

Dotted symbol evaluation traverses object and module properties hierarchically using dot notation.

## Dotted Symbol Structure

**Input IR:** `DottedSymbol [part1, part2, ...]`
**Process:** Traverse object/module properties hierarchically
**Output:** Final property value or error

### Syntax
```reactor
obj.field           ;; Access 'field' on object 'obj'
module.func         ;; Access 'func' export from module
data.user.name      ;; Deep property access
```

## Evaluation Process

### Traversal Algorithm
1. **Resolve first part** as symbol in current environment
2. **For each subsequent part:**
   - Access property/export on current object/module
   - Update current value to the accessed result
3. **Return final value**

### Prefix Resolution
For complex dotted paths like `a.b.c.d`:
- Try `a.b.c.d` as a single symbol first
- Fall back to `a.b.c`, then `a.b`, then `a`
- Use the longest matching prefix as starting point

## Property vs Export Access

### Objects
- **Access:** Direct key lookup in `Object (Map Text IR)`
- **Errors:** `PropertyNotFound` when key doesn't exist
- **Evaluation:** Property values are pre-evaluated

### Modules
- **Access:** Lookup in `Module (Map Text IR)` export map
- **Errors:** Should use `ExportNotFound` (different from `PropertyNotFound`)
- **Evaluation:** Module evaluated once at import time
- **Caching:** Results cached to avoid re-evaluation

## Traversal Examples

```reactor
;; Object property access
(:name "Alice" :age 25).name  ;; → "Alice"

;; Module export access
math.add  ;; Access 'add' from math module

;; Deep traversal
user.profile.settings.theme  ;; Nested access

;; Mixed object/module
config.database.connection.url  ;; Module → Object → Property
```

## Error Conditions

### PropertyNotFound
**Cause:** Object property doesn't exist
**Context:** Property name, object being accessed

### ExportNotFound
**Cause:** Module export doesn't exist
**Context:** Export name, module being accessed

### NotAnObject
**Cause:** Attempted property access on non-object value
**Context:** Value type, attempted property name

### Circular Dependencies
**Cause:** Module traversal creates circular reference
**Context:** Module chain causing the cycle

## Performance Characteristics

### Access Cost
- **Simple:** O(depth + path_length)
- **Complex:** Multiple environment lookups for prefix resolution
- **Cached:** Module results avoid re-evaluation

### Optimization Opportunities
- **Prefix caching:** Cache resolved prefixes
- **Property indexing:** Fast map lookups
- **Module caching:** Avoid re-evaluation of imported modules

## Implementation Details

### Resolution Strategy
```haskell
evalDottedSymbol :: [Text] -> Eval (Maybe IR)
evalDottedSymbol parts = do
    case parts of
        [] -> throwError EmptyDottedPath
        [single] -> evalSymbol single
        _ -> evalWithPrefixes (init $ inits parts)
```

### Property Access
```haskell
accessProperty :: IR -> Text -> Eval (Maybe IR)
accessProperty (IR.Object map) prop =
    case Map.lookup prop map of
        Just val -> pure (Just val)
        Nothing -> throwError $ PropertyNotFound prop
accessProperty (IR.Module map) prop =
    case Map.lookup prop map of
        Just val -> pure (Just val)
        Nothing -> throwError $ ExportNotFound prop  -- Should be different error
accessProperty _ _ = throwError NotAnObject
```

## Common Patterns

### Configuration Access
```reactor
config.database.host     ;; Database host
config.logging.level     ;; Log level
settings.ui.theme        ;; UI theme
```

### API Access
```reactor
http.get                 ;; HTTP GET function
json.parse               ;; JSON parser
fs.readFile              ;; File system access
```

### Data Structure Navigation
```reactor
response.data.users[0].name    ;; API response navigation
document.body.children[1]      ;; DOM traversal
```

## See Also

- [Symbol Evaluation](EVALUATION_SYMBOLS.md) - Simple symbol resolution
- [Environment](ENVIRONMENT.md) - Variable binding context
- [Evaluation](EVALUATION.md) - Main evaluation overview
