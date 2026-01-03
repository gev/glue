# Bool Module

The bool module provides boolean values, comparison operations, and control flow constructs for Glue programs.

## Boolean Values

- `true` - Boolean true value
- `false` - Boolean false value

## Comparison Operations

### Equality and Inequality
- `eq` / `==` - Test equality between two values
- `ne` / `!=` - Test inequality between two values

### Ordering Comparisons
- `lt` / `<` - Test if first value is less than second
- `le` / `<=` - Test if first value is less than or equal to second
- `gt` / `>` - Test if first value is greater than second
- `ge` / `>=` - Test if first value is greater than or equal to second

## Logical Operations

- `not` / `!` - Logical negation

## Control Flow

- `if` - Conditional execution
- `when` - Execute body if condition is true
- `while` - Execute body while condition is true
- `until` - Execute body until condition becomes true

## Examples

```
(if (eq x 5) "equal to 5" "not equal to 5")
(when (< count 10) (set count (+ count 1)))
(while (not done) (process-item))
```

## See Also

- [Standard Library Introduction](STDLIB_INTRO.md)
- [Builtin Module](STDLIB_BUILTIN.md)
