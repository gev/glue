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

## Evaluation Process

With the environment prepared, Reactor can begin evaluating code. The evaluation process recursively processes different types of IR nodes according to their semantic meaning:

### Symbol Evaluation

When encountering a symbol, Reactor looks up its value in the current environment. This includes both simple variable names and dotted notation for accessing nested properties or module exports. Symbol evaluation forms the foundation of variable access and function calls. For detailed information about environment structure and symbol resolution, see [Environment](ENVIRONMENT.md).

### List Evaluation

List evaluation analyzes the input list structure and transforms it into a callable IR if possible, otherwise evaluates list elements. The process follows these cases:

**Single Symbol Lists:**
- **Input:** `List [Symbol name]`
  **Process:** Look up `name` in environment. If callable, apply with no arguments. If not callable, return the looked-up value.

**Symbol with Arguments:**
- **Input:** `List [Symbol name, arg1, arg2, ...]`
  **Process:** Look up `name` in environment and apply the result to unevaluated `arg1`, `arg2`, ...`

**General Lists:**
- **Input:** `List [item1, item2, ...]`
  **Process:** Evaluate all items. If first evaluated item is callable, apply it to remaining evaluated items. Otherwise return list of all evaluated items.

This analysis-first approach enables efficient function detection without unnecessary evaluation. For comprehensive details about function application and data structures, see [Function Application](EVALUATION_FUNCTIONS.md) and [Data Structures](EVALUATION_DATA.md).

### Object Evaluation

Objects represent structured data with named properties. During evaluation, all property values are recursively evaluated, creating a fully resolved data structure. Objects support dynamic property access and form the basis for complex data modeling. For detailed information about object evaluation and manipulation, see [Data Structures](EVALUATION_DATA.md).

### Literal Evaluation

Literal values such as numbers, strings, and other primitive types evaluate to themselves. These self-evaluating forms provide the atomic building blocks of Reactor programs. For information about all supported primitive types, see [Primitive Values](EVALUATION_PRIMITIVES.md).

## Special forms and operators evaluation

To evaluate special forms and operators in Reactor programs, the runtime relies on the standard library for their implementation. Special forms like `import`, `def`, `lambda`, `set`, `quote`, and `if`, along with boolean values `true` and `false`, and operators such as `+`, `-`, `/`, `%`, `*`, `**`, `<`, `<=`, `==`, `=>`, `>`, `\`, and others, are all defined in the standard library modules.

The standard library provides the essential functionality that makes Reactor programs executable. Without these library definitions, special forms and operators would not be available during evaluation. For detailed documentation of all available modules and their functions, see the [Standard Library Documentation](STDLIB_INTRO.md).
