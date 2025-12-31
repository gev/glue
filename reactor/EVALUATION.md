# Reactor IR Evaluation

## Overview

Reactor's evaluation process transforms Intermediate Representation (IR) nodes into runtime values through the interpreter. This evaluation step executes Reactor programs by recursively processing IR nodes according to their types.

## Evaluation Cases

Reactor evaluates different types of IR nodes through specific evaluation rules. Each case is documented in detail in separate files:

### Primitive Values
- **Numbers** - Self-evaluating numeric literals
- **Strings** - Self-evaluating text literals
- **[Detailed Documentation](EVALUATION_PRIMITIVES.md)**

### Symbol Resolution
- **Simple Symbols** - Variable and function name lookup
- **Dotted Symbols** - Hierarchical property access
- **[Detailed Documentation](EVALUATION_SYMBOLS.md)**

### Function Application
- **Native Functions** - Host language function calls
- **Closures** - User-defined function execution
- **Symbol Functions** - Dynamic function resolution
- **[Detailed Documentation](EVALUATION_FUNCTIONS.md)**

### Data Structures
- **Lists** - Sequence evaluation and function application
- **Objects** - Property evaluation and construction
- **[Detailed Documentation](EVALUATION_DATA.md)**

### Special Forms
- **Modules** - Registration and import handling
- **[Detailed Documentation](EVALUATION_MODULES.md)**
- **Def** - Variable definition
- **[Detailed Documentation](EVALUATION_DEF.md)**
- **Lambda** - Function creation
- **[Detailed Documentation](EVALUATION_LAMBDA.md)**
- **Quote** - Data literals
- **[Detailed Documentation](EVALUATION_QUOTE.md)**
- **Set** - Variable/property mutation
- **[Detailed Documentation](EVALUATION_SET.md)**

## Evaluation Process

The evaluation follows a recursive pattern:

1. **Match IR node type** to appropriate evaluation rule
2. **Apply rule** to transform node into result value
3. **Handle errors** with context and location information
4. **Return result** or propagate errors up the call stack

## Key Concepts

### Environment
Evaluation occurs within an environment that provides variable bindings and scoping.

### Context
Call stack tracking enables precise error reporting and debugging.

### Recursion
Complex IR structures are evaluated by recursively processing their components.

### Laziness
Arguments and module bodies are evaluated only when needed.

## Error Handling

Evaluation can produce various errors:
- Variable not found in environment
- Property access on non-objects
- Wrong number of function arguments
- Type mismatches in operations

All errors include evaluation context for debugging.
