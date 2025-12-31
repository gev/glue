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
For a dotted path like `a.b.c.d`:

1. **Look up `a`** as symbol in environment
2. **If `a` is object:** Look up property `b` on `a`
3. **If `a` is module:** Look up export `b` from `a`
4. **Continue with `c`** on the result from step 2/3
5. **Repeat for `d`**
6. **If current value is not object/module:** Fail with type-specific error

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
**Cause:** Attempted property access on value that is not an object or module
**Context:** Actual value type (string, number, function, etc.), attempted property name
**Examples:**
- `"hello".invalid` → NotAnObject with type "string"
- `42.field` → NotAnObject with type "number"
- `(fn [] 1).prop` → NotAnObject with type "closure"

### Circular Dependencies
**Cause:** Module traversal creates circular reference
**Context:** Module chain causing the cycle

## Performance Characteristics

### Access Cost
- **Simple:** O(depth + path_length)
- **Worst case:** One environment lookup + N property lookups for N path components
- **Cached:** Module results avoid re-evaluation

### Optimization Opportunities
- **Prefix caching:** Cache resolved prefixes
- **Property indexing:** Fast map lookups
- **Module caching:** Avoid re-evaluation of imported modules

## Implementation Details

### Resolution Strategy
The evaluation follows a sequential approach:
1. Split the dotted path into components: `["a", "b", "c", "d"]`
2. Look up the first component `"a"` as a symbol
3. For each remaining component, perform property access on the current value
4. Each step validates the current value is an object or module

### Property Access Logic
- **Object Access:** Direct key lookup in the object's property map
- **Module Access:** Lookup in the module's export table
- **Type Checking:** Ensure the base value is an object or module before access
- **Error Differentiation:** Use appropriate error types for different access failures

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
