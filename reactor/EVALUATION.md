# Reactor Evaluation

## Overview

Reactor's evaluation process transforms Intermediate Representation (IR) code into executable runtime values. This document describes the complete evaluation lifecycle, from initial preparation through execution of different code constructs.

## Evaluation Preparation

Before any code can be evaluated, the Reactor runtime environment must be properly established. This preparation phase ensures that all necessary components are available for program execution.

### Module Registration

The first step in preparation is module registration, where Reactor modules are made available to the evaluation system. Each module defines its exported symbols and implementation details. Module registration establishes the available code libraries that programs can import and use. For detailed information about the module registration process, see [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md).

### Environment Preparation

Environment preparation establishes the variable bindings and scoping context that will be used during evaluation. The environment contains all the symbols (variables, functions, constants) that code can reference. Reactor supports different types of environments:

- **Standard Environment**: Includes the complete standard library with all builtin functions and operators
- **Custom Environment**: Contains specific bindings tailored for particular execution contexts
- **Minimal Environment**: Provides only essential operations for constrained execution

The environment serves as the foundation for symbol resolution during evaluation. For detailed information about environment structure and frame stack management, see [Environment](ENVIRONMENT.md).

### Evaluation State Preparation

Once modules are registered and the environment is prepared, the initial EvalState must be constructed. The EvalState provides the complete runtime context for evaluation, including environment, call stack, module registries, and caching infrastructure. Reactor supports different EvalState configurations for various execution scenarios. For comprehensive details about EvalState construction and component initialization, see [EvalState Preparation](EVALUATION_PREPARATION_EVALSTATE.md).

## Evaluation Rules

Reactor evaluation follows a pattern-matching approach where each IR node type is handled by a specific evaluation case:

### Symbol Evaluation
Resolves identifiers to their bound values in the environment. For detailed information, see [Symbol Evaluation](EVALUATION_SYMBOLS.md).

### List Evaluation
Analyzes list structure for function calls or data. For detailed information, see [List Evaluation](EVALUATION_LISTS.md).

### Object Evaluation
Evaluates property values while preserving keys. For detailed information, see [Object Evaluation](EVALUATION_OBJECTS.md).

### Literal Evaluation
Handles self-evaluating values. For detailed information, see [Literal Evaluation](EVALUATION_LITERALS.md).

### Function Application
All function application logic is handled within the list evaluation cases. For comprehensive details about function calling semantics, see [Function Application](EVALUATION_FUNCTIONS.md).

## Error Conditions

- **UnboundVariable**: Symbol not found in environment
- **NotCallableObject**: Attempted to call non-callable value
- **WrongNumberOfArguments**: Parameter/argument count mismatch
- **PropertyNotFound**: Object/module property access failed
- **NotAnObject**: Dotted access on non-object value

## Module System

### Module Registration
- Store module metadata (name, exports, body)
- Body remains unevaluated until import

### Module Import
- **First Import**: Evaluate module body in isolated environment, cache results
- **Subsequent Imports**: Return cached results directly
- Merge exported symbols into importing environment

### Environment Isolation
- Modules evaluate in separate environment branch
- Cannot access importing scope variables
- Share common builtin functions

## Evaluation Order

- **Arguments**: Evaluated left-to-right before function application
- **List Elements**: Evaluated left-to-right for data lists
- **Object Properties**: Property values evaluated (order not guaranteed)
- **Nested Structures**: Evaluation proceeds depth-first

## Special Considerations

### Call Stack Management
- Push function names before evaluation
- Pop after completion
- Include in error messages for debugging

### Lazy vs Eager Evaluation
- Reactor uses eager evaluation for all arguments
- No lazy evaluation of unused expressions
- Consistent with function argument evaluation

### Immutability
- Evaluated data structures are immutable
- New structures created for each evaluation
- Supports functional programming patterns

### Type System
- Dynamic typing with runtime type checks
- IR types provide structural typing
- Host language integration through Native functions

## Implementation Requirements

To implement Reactor evaluation, a host language must provide:

1. **IR Data Structures**: All IR types with proper equality/comparison
2. **Environment Management**: Frame stack with lookup operations
3. **Error Handling**: All specified error conditions
4. **Function Application**: Native function integration
5. **Module System**: Registration, import, and caching
6. **Evaluation Loop**: Recursive evaluation of IR expressions

This specification enables Reactor implementation in any Turing-complete programming language.

## Special forms and operators evaluation

To evaluate special forms and operators in Reactor programs, the runtime relies on the standard library for their implementation. Special forms like `import`, `def`, `lambda`, `set`, `quote`, and `if`, along with boolean values `true` and `false`, and operators such as `+`, `-`, `/`, `%`, `*`, `**`, `<`, `<=`, `==`, `=>`, `>`, `\`, and others, are all defined in the standard library modules.

The standard library provides the essential functionality that makes Reactor programs executable. Without these library definitions, special forms and operators would not be available during evaluation. For detailed documentation of all available modules and their functions, see the [Standard Library Documentation](STDLIB_INTRO.md).
