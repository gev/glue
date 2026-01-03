# Evaluation: Primitive Values

## Overview

Primitive values in Reactor are self-evaluating - they return themselves unchanged during evaluation.

## Number Evaluation

**Input IR:** `Number n`
**Process:** No evaluation needed
**Output:** Returns the same `Number n`
**Example:** `Number 42.0` → `Number 42.0`

Numbers represent all numeric values including integers, decimals, and scientific notation. They pass through evaluation unchanged since they are already in their final runtime form.

## String Evaluation

**Input IR:** `String s`
**Process:** No evaluation needed
**Output:** Returns the same `String s`
**Example:** `String "hello"` → `String "hello"`

Strings represent text literals with support for escape sequences. Like numbers, they are self-evaluating and return unchanged.

## Characteristics

### Self-Evaluating
Primitive values do not require any computation or environment lookup. They represent their final runtime values directly.

### Immutable
Once created, primitive values cannot be modified. Operations on primitives create new values.

### Direct Representation
Primitives map directly to their runtime representations without transformation.

## Error Conditions

Primitive evaluation cannot fail since no computation is performed. Invalid primitive creation is caught during parsing or compilation, not evaluation.
