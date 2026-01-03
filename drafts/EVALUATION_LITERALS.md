# Literal Evaluation

Literal evaluation handles self-evaluating values that return themselves without further processing.

## Supported Literal Types

### Numbers
**Input IR:** `Number value`
**Process:** Return value unchanged
**Examples:** `42`, `3.14`, `-10`

### Strings
**Input IR:** `String value`
**Process:** Return value unchanged
**Examples:** `"hello"`, `"world"`, `""`

### Other Literals
- **Booleans** (if represented as literals)
- **Null/None** values (if present)
- **Constants** and **symbols** that evaluate to themselves

## Evaluation Process

**Input:** Any IR node not handled by specific cases
**Process:** Return the IR node unchanged
**Output:** Original IR value

### Default Case
```haskell
eval :: IR -> Eval (Maybe IR)
-- ... specific cases ...
eval v = pure (Just v)  -- Literals return themselves
```

## Literal Characteristics

### Self-Evaluating
- No further evaluation needed
- Atomic values in the language
- Terminal points in expression trees

### Immutability
- Literal values cannot be modified
- Always return the same value
- Safe for sharing and reuse

### Type Preservation
- Original type information maintained
- No type coercion or conversion
- Exact representation preserved

## Examples

```closure
;; Numbers
42        ;; → 42
3.14159   ;; → 3.14159

;; Strings
"hello"   ;; → "hello"
""        ;; → ""

;; In expressions
(+ 1 2)   ;; Numbers 1, 2 evaluate to themselves
(list "a" "b")  ;; Strings evaluate to themselves
```

## Performance Characteristics

### Evaluation Cost
- **O(1)**: No computation required
- **Instant return**: Direct value pass-through
- **Zero overhead**: Most efficient evaluation case

### Memory Usage
- **No allocation**: Original value reused
- **Reference sharing**: Safe for multiple references
- **Minimal footprint**: Just the value itself

## Implementation Details

### Pattern Matching
```haskell
eval (Number n) = pure (Just (Number n))
eval (String s) = pure (Just (String s))
eval other = evalComplex other  -- Delegate to specific handlers
```

### Default Fallback
```haskell
eval v = pure (Just v)  -- Catch-all for literals
```

## Special Considerations

### Evaluation Order
- Literals evaluated before complex expressions
- Provide base cases for recursive evaluation
- Ensure termination of evaluation process

### Serialization
- Literals can be directly serialized
- No evaluation context needed
- Portable across implementations

### Equality
- Literal equality based on value
- Numbers: Numeric equality
- Strings: Lexical equality
- Consistent across evaluation contexts

## Error Conditions

Literals cannot produce evaluation errors:
- Always succeed
- No external dependencies
- No complex evaluation logic

## Language Design Implications

### Atomic Values
- Literals form the foundation of the language
- All complex values build from literals
- Provide basic data types

### Evaluation Termination
- Literals ensure evaluation always terminates
- Base cases for recursive evaluation functions
- Prevent infinite evaluation loops

### Optimization Opportunities
- Literals can be constant-folded
- No need for re-evaluation
- Cacheable and shareable

## See Also

- [Evaluation](EVALUATION.md) - Main evaluation overview
- [Data Types](EVALUATION.md#data-types) - IR type system
- [Primitives](EVALUATION_PRIMITIVES.md) - Primitive value handling
