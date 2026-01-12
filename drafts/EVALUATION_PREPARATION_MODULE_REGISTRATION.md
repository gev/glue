# Module Registration

Module registration is the process of making Glue modules available to the evaluation system before program execution begins.

## Process

1. **Module Definition** - Modules are defined with their name, exported symbols, and body forms
2. **Registry Storage** - Modules are stored in the module registry for lookup during import
3. **Export Declaration** - Each module declares which symbols it makes available to importing code

## Registration Requirements

- Module name must be unique within the registry
- All exported symbols must be defined within the module's body
- Module body consists of forms that are evaluated when the module is imported

## See Also

- [Runtime Preparation](EVALUATION_PREPARATION_RUNTIME.md)
- [Standard Library Documentation](../STDLIB_INTRO.md)
