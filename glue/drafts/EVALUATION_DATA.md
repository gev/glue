# Data Structure Evaluation

Data structure evaluation processes lists and objects by recursively evaluating their components. Unlike function application, data structures return constructed values rather than executing operations.

## List Evaluation

**Input IR:** `List elements`
**Process:** Evaluate all elements, construct new list
**Output:** New list with evaluated elements

### Evaluation Process
1. Evaluate each element in the list
2. Collect evaluated results
3. Return new `List` with evaluated elements

### Function vs Data Distinction
Lists serve dual purpose:
- **Function calls:** When first element is callable
- **Data lists:** When first element is not callable

### Examples
- `List [Number 1, Number 2, Number 3]` → `List [Number 1, Number 2, Number 3]`
- `List [String "a", String "b"]` → `List [String "a", String "b"]`

## Object Evaluation

**Input IR:** `Object properties`
**Process:** Evaluate all property values, construct new object
**Output:** New object with evaluated properties

### Evaluation Process
1. Evaluate each property value in the object
2. Preserve property keys unchanged
3. Return new `Object` with evaluated values

### Property Preservation
- Property names (keys) remain as text literals
- Only property values are evaluated
- Structure and keys maintain original form

### Examples
```reactor
(:name "Alice" :age (+ 20 5))
```
**Input:** `Object [("name", String "Alice"), ("age", List [Symbol "+", Number 20, Number 5])]`
**Output:** `Object [("name", String "Alice"), ("age", Number 25)]`

## Recursive Evaluation

### Deep Evaluation
Data structures evaluate recursively:
- List elements can be other lists or objects
- Object values can contain nested structures
- Evaluation continues until primitive values reached

### Complex Nesting
```reactor
(:data (1 2 (:nested "value")) :func (+ 1 2))
```
Evaluates to fully resolved nested structure with computed values.

## Error Propagation

### Element Evaluation Errors
If any element evaluation fails:
- Error propagates up immediately
- Partial results discarded
- Full context preserved in error

### Type Consistency
- Evaluated elements maintain type relationships
- Structure integrity preserved
- No type coercion during evaluation

## Performance Considerations

### Lazy Evaluation
Data structures could potentially use lazy evaluation, but Reactor currently uses eager evaluation for consistency with function arguments.

### Memory Usage
- New structures created for each evaluation
- Original IR nodes remain unchanged
- Memory growth proportional to structure depth

## Special Cases

### Empty Structures
- Empty lists: `List []` → `List []`
- Empty objects: `Object []` → `Object []`
- No evaluation needed, return unchanged

### Homogeneous Data
Lists can contain mixed types:
- Numbers, strings, other lists, objects
- Type safety maintained through IR type system
- No runtime type checking during construction

### Immutable Results
Evaluated data structures are immutable:
- Cannot be modified after creation
- New operations create new structures
- Supports functional programming patterns
