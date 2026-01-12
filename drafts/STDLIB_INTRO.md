# Glue Standard Library

The Glue standard library provides essential functions and data types for Glue program development.

## Available Modules

### Core Modules
- **[Builtin](STDLIB_BUILTIN.md)** - Essential special forms and core functionality
- **[Bool](STDLIB_BOOL.md)** - Boolean operations and control flow

### Additional Modules
- **List** - List manipulation functions
- **Math** - Mathematical operations and arithmetic
- **Arithmetic** - Advanced numeric operations

## Module Loading

Modules can be imported into programs using the `import` special form:

```
(import "module-name")
```

This makes all exported symbols from the module available in the current environment.

## See Also

- [Evaluation Documentation](EVALUATION.md)
- [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md)
- [Runtime Preparation](EVALUATION_PREPARATION_EVALSTATE.md)
