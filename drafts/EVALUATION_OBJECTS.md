# Object Evaluation

Object evaluation processes key-value mappings by evaluating all property values while preserving keys.

## Object Structure

**Input IR:** `Object [("key1", value1), ("key2", value2), ...]`
**Process:** Evaluate each `value`, preserve each `"key"`
**Output:** `Object [("key1", evaluated_value1), ("key2", evaluated_value2), ...]`

### Key Preservation
- Property names remain as string literals
- Keys are not evaluated or modified
- Original key order maintained

### Value Evaluation
- Each property value evaluated recursively
- Complex expressions computed to final values
- Nested objects/lists evaluated deeply

## Evaluation Process

1. **Start** with input object map
2. **For each property:**
   - Keep key unchanged
   - Evaluate value using main `eval` function
   - Collect evaluated value
3. **Construct** new object with preserved keys and evaluated values
4. **Return** new `Object` IR node

## Object Evaluation Examples

```closure
;; Simple object
(:name "Alice" :age 25)
;; → Object [("name", String "Alice"), ("age", Number 25)]

;; Object with expressions
(:total (+ 10 20) :message (str "Hello" "World"))
;; → Object [("total", Number 30), ("message", String "HelloWorld")]

;; Nested objects
(:user (:name "Bob" :id (+ 100 5)))
;; → Object [("user", Object [("name", String "Bob"), ("id", Number 105)])]
```

## Host Object Property Access

Host objects (Haskell data types) support property access using dot notation.

### Haskell Data Types
```haskell
data Person = Person {name :: Text, age :: Int, address :: Maybe Address}
data Address = Address {street :: Text, city :: Text}
```

### Basic Property Access
```glue
;; Create host objects
(def bob (person :name "Bob" :age 25))
(def addr (address :street "123 Main St" :city "Springfield"))

;; Access properties
bob.name        ;; → "Bob"
bob.age         ;; → 25
addr.street     ;; → "123 Main St"
addr.city       ;; → "Springfield"
```

### Nested Property Access
```glue
;; Create nested relationship
(def bob (person :name "Bob" :age 25 :address addr))

;; Access nested properties
bob.address.city        ;; → "Springfield"
bob.address.street      ;; → "123 Main St"

;; Deep nesting
(def company (:name "ACME" :hq (:city "Springfield" :state "IL")))
company.hq.city         ;; → "Springfield"
company.hq.state        ;; → "IL"
```

### Property Access on Optional Values
```glue
;; Handle optional nested objects
(def person-no-address (person :name "Alice" :age 30))
person-no-address.address   ;; → Error: property not found

;; Safe access patterns require explicit null checking
(if person-no-address.address
    person-no-address.address.city
    "No address")
```

## Error Propagation

### Value Evaluation Errors
- If any property value evaluation fails
- Error propagates up immediately
- Partial results discarded
- Full evaluation context preserved

### Property Access Errors
- Errors in nested property access
- Errors in function calls within values
- All errors maintain object context

## Performance Characteristics

### Evaluation Cost
- O(n) where n is number of properties
- Each property value evaluated independently
- No inter-property dependencies

### Memory Usage
- New object map created
- Original object unchanged
- Property values may create new structures

### Parallel Evaluation
- Properties could be evaluated in parallel
- Currently evaluated sequentially
- Order not guaranteed (implementation detail)

## Implementation Details

### Object Map Structure
```haskell
type ObjectMap = Map Text IR
data IR = ... | Object ObjectMap | ...
```

### Evaluation Function
```haskell
evalObject :: Map Text IR -> Eval (Maybe IR)
evalObject objMap = do
    evaluatedMap <- mapM eval objMap
    let cleanMap = Map.mapMaybe id evaluatedMap
    pure (Just (Object cleanMap))
```

### Map Traversal
- `mapM eval` applies `eval` to each value
- `Map.mapMaybe id` filters out `Nothing` results
- Preserves key structure

## Special Cases

### Empty Objects
**Input:** `Object []`
**Process:** No evaluation needed
**Output:** `Object []`

### Computed Property Names
- Property names are static strings
- No dynamic property name computation
- Keys must be compile-time constants

### Property Name Conflicts
- Duplicate keys: Last occurrence wins
- Key uniqueness enforced by map structure
- No runtime key collision detection

## Object Operations

### Property Access
```closure
(:x 1 :y 2).x  ;; → 1
(:x 1 :y 2).z  ;; Error: PropertyNotFound
```

### Object Construction
```closure
(def obj (:a 1 :b 2))  ;; Create object
(set obj.c 3)          ;; Add property
```

### Object Functions
- Standard library provides object manipulation functions
- Property access, merging, transformation
- All operate on evaluated objects

## Type Safety

### Runtime Type Checking
- Property values can be any IR type
- No type restrictions on values
- Type safety through IR type system

### Structural Typing
- Objects are structurally typed
- No nominal type system
- Property presence checked at access time

## See Also

- [Data Structures](EVALUATION_DATA.md) - General data evaluation
- [Evaluation](EVALUATION.md) - Main evaluation overview
- [Environment](ENVIRONMENT.md) - Variable binding context
