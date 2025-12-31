# Reactor Evaluation

## Overview

Reactor's evaluation process transforms Intermediate Representation (IR) code into executable runtime values. This document describes the complete evaluation lifecycle, from initial preparation through execution of different code constructs.

## Evaluation Preparation

Before any code can be evaluated, the Reactor runtime environment must be properly established. This preparation phase ensures that all necessary components are available for program execution.

### Module Registration

The first step in preparation is module registration, where Reactor modules are made available to the evaluation system. Each module defines its exported symbols and implementation details. Module registration establishes the available code libraries that programs can import and use. For detailed information about the module registration process, see [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md).

### Evaluation State Preparation

Once modules are registered, the initial EvalState must be constructed. The EvalState provides the complete runtime context for evaluation, including environment, call stack, module registries, and caching infrastructure. Reactor supports different EvalState configurations for various execution scenarios. For comprehensive details about EvalState construction and component initialization, see [EvalState Preparation](EVALUATION_PREPARATION_EVALSTATE.md).

## Evaluation Process

With the environment prepared, Reactor can begin evaluating code. The evaluation process recursively processes different types of IR nodes according to their semantic meaning:

### Symbol Evaluation

When encountering a symbol, Reactor looks up its value in the current environment. This includes both simple variable names and dotted notation for accessing nested properties or module exports. Symbol evaluation forms the foundation of variable access and function calls. For detailed information about symbol resolution mechanisms, see [Symbol Evaluation](EVALUATION_SYMBOLS.md).

### List Evaluation

Lists in Reactor serve dual purposes: they can represent literal data structures or function applications. When a list is evaluated, Reactor first evaluates its first element to determine if it's callable, then applies it to the remaining evaluated arguments. This mechanism enables both data manipulation and computational operations. For comprehensive details about function application and data structures, see [Function Application](EVALUATION_FUNCTIONS.md) and [Data Structures](EVALUATION_DATA.md).

### Object Evaluation

Objects represent structured data with named properties. During evaluation, all property values are recursively evaluated, creating a fully resolved data structure. Objects support dynamic property access and form the basis for complex data modeling. For detailed information about object evaluation and manipulation, see [Data Structures](EVALUATION_DATA.md).

### Literal Evaluation

Literal values such as numbers, strings, and other primitive types evaluate to themselves. These self-evaluating forms provide the atomic building blocks of Reactor programs. For information about all supported primitive types, see [Primitive Values](EVALUATION_PRIMITIVES.md).

## Special forms and operators evaluation

To evaluate special forms and operators in Reactor programs, the runtime relies on the standard library for their implementation. Special forms like `import`, `def`, `lambda`, `set`, `quote`, and `if`, along with boolean values `true` and `false`, and operators such as `+`, `-`, `/`, `%`, `*`, `**`, `<`, `<=`, `==`, `=>`, `>`, `\`, and others, are all defined in the standard library modules.

The standard library provides the essential functionality that makes Reactor programs executable. Without these library definitions, special forms and operators would not be available during evaluation. For detailed documentation of all available modules and their functions, see the [Standard Library Documentation](STDLIB_INTRO.md).
