# Environment Preparation

Environment preparation establishes the execution context for Reactor programs by setting up variable bindings and scoping rules.

## Environment Types

### Empty Environment
An empty environment contains no predefined bindings. Programs must define all symbols they use or import them from modules.

### Standard Environment
The standard environment includes the complete standard library with all builtin functions, data types, and module system support.

### Custom Environments
Custom environments can be created with specific sets of bindings for specialized execution contexts:

- **Minimal Environment** - Contains only essential builtin forms (def, lambda, quote)
- **Domain-Specific Environment** - Includes bindings tailored for specific application domains
- **Sandbox Environment** - Restricted environment for safe code execution

## Environment Structure

Environments are organized as frames that can be pushed and popped to manage scoping:

- **Global Frame** - Contains builtin functions and constants
- **Module Frames** - Contain imported module bindings
- **Local Frames** - Contain function parameters and local variables

## Preparation Process

1. **Base Environment Creation** - Start with empty or standard environment
2. **Module Loading** - Import required modules and merge their exports
3. **Custom Binding Addition** - Add any additional bindings needed for the program
4. **Environment Validation** - Ensure all required symbols are available

## See Also

- [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md)
- [Standard Library Documentation](../STDLIB_INTRO.md)
