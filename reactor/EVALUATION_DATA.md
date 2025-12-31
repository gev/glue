# Data Structure Evaluation

Data structures evaluate their elements recursively while preserving structure.

## Lists

Lists as data evaluate all elements recursively.

**Input:** `List [item1, item2, ...]`
**Process:** Evaluate each element, return new list with evaluated elements

**Note:** Lists are treated as data only when they don't represent function calls.

## Objects

Objects evaluate property values while preserving keys.

**Input:** `Object [("key1", value1), ("key2", value2), ...]`
**Process:** Evaluate each value, return new object with evaluated values

**Note:** Property names remain unchanged, only values are evaluated.

## Recursive Evaluation

Both lists and objects evaluate nested structures:
- List elements can contain other lists or objects
- Object values can be complex expressions
- Evaluation continues until all primitive values are reached

## Examples

```reactor
;; List evaluation
(1 (+ 2 3) "hello")  ;; → (1 5 "hello")

;; Object evaluation
(:name "Alice" :age (+ 20 5))  ;; → (:name "Alice" :age 25)
```
