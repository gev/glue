# Data Structure Evaluation

## Lists

Lists are evaluated as data when they don't represent function calls. All elements are recursively evaluated.

**Input:** `List [item1, item2, ...]`
**Process:** Evaluate each `item`, return `List [evaluated_item1, evaluated_item2, ...]`

## Objects

Objects evaluate their property values while preserving keys.

**Input:** `Object [("key1", value1), ("key2", value2), ...]`
**Process:** Evaluate each `value`, return `Object [("key1", evaluated_value1), ("key2", evaluated_value2), ...]`
