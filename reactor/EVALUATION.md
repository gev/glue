# Reactor Evaluation

## Overview

Reactor's evaluation process transforms Intermediate Representation (IR) code into executable runtime values. This document describes the complete evaluation lifecycle, from initial preparation through execution of different code constructs.

## Evaluation Preparation

Before any code can be evaluated, the Reactor runtime environment must be properly established. This preparation phase ensures that all necessary components are available for program execution.

### Module Registration

The first step in preparation is module registration, where Reactor modules are made available to the evaluation system. Each module defines its exported symbols and implementation details. Module registration establishes the available code libraries that programs can import and use. For detailed information about the module registration process, see [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md).

### Environment Preparation

Once modules are registered, the evaluation environment must be configured. The environment provides the context in which code executes, including variable bindings, function definitions, and scoping rules. Reactor supports different types of environments - from minimal setups containing only essential operations to full standard environments with complete library access. Environment preparation can include custom configurations for specific use cases, such as sandboxed execution or domain-specific functionality. For comprehensive details about environment setup and configuration options, see [Environment Preparation](EVALUATION_PREPARATION_ENVIRONMENT.md).

## Evaluation Process

With the environment prepared, Reactor can begin evaluating code. The evaluation process recursively processes different types of IR nodes according to their semantic meaning:

### Symbol Evaluation

When encountering a symbol, Reactor looks up its value in the current environment. This includes both simple variable names and dotted notation for accessing nested properties or module exports. Symbol evaluation forms the foundation of variable access and function calls.

### List Evaluation

Lists in Reactor serve dual purposes: they can represent literal data structures or function applications. When a list is evaluated, Reactor first evaluates its first element to determine if it's callable, then applies it to the remaining evaluated arguments. This mechanism enables both data manipulation and computational operations.

### Object Evaluation

Objects represent structured data with named properties. During evaluation, all property values are recursively evaluated, creating a fully resolved data structure. Objects support dynamic property access and form the basis for complex data modeling.

### Literal Evaluation

Literal values such as numbers, strings, and other primitive types evaluate to themselves. These self-evaluating forms provide the atomic building blocks of Reactor programs.

## Standard Library

Reactor includes a comprehensive standard library that provides essential functionality for program development. The standard library is organized into modules, each focusing on specific domains of functionality. Core modules include fundamental operations, boolean logic, mathematical functions, and data structure manipulation. For detailed documentation of available modules and their functions, see the [Standard Library Documentation](STDLIB_INTRO.md).
