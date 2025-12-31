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

### Primitive Values

Numbers, strings, and other literals evaluate to themselves.

### Symbol Evaluation

1. Search environment frames from top to bottom
2. Return bound value or raise UnboundVariable error
3. Dotted symbols traverse object/module properties

### List Evaluation

Lists are evaluated based on their first element:

#### Case 1: Single Symbol `[symbol]`
- Look up symbol in environment
- If result is callable: apply with no arguments
- If result is not callable: return the value
- Error if symbol not found

#### Case 2: Symbol with Arguments `[symbol, arg1, arg2, ...]`
- Look up symbol in environment
- Apply result to unevaluated arguments
- Error if symbol not found

#### Case 3: General List `[item1, item2, ...]`
- Evaluate all items in the list
- If first evaluated item is callable: apply it to remaining evaluated items
- Otherwise: return new list with all evaluated items

### Object Evaluation

1. Evaluate all property values
2. Preserve property keys unchanged
3. Return new object with evaluated values

### Function Application

#### Native Functions
- Evaluate all arguments first (eager evaluation)
- Pass evaluated arguments to native function
- Return function result

#### Closures
- Evaluate all arguments first
- Create new environment frame
- Bind parameters to evaluated arguments
- Evaluate closure body in extended environment
- Return body evaluation result

#### Symbol Resolution
- Look up symbol in current environment
- Apply resolved value if callable
- Error if not callable or not found

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
