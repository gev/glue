# Reactor Language Specification

This specification defines the Reactor programming language, designed for embedding in host applications across multiple platforms including Haskell, Dart, TypeScript, and others.

## Overview

Reactor is a Lisp-inspired scripting language that serves as a universal controller for domain objects, DTOs, and business logic. It provides a safe, expressive way to manipulate data and call host language functions through a clean functional programming paradigm.

## Specification Structure

This specification is organized as follows:

- **[Language Aims](language-aims.md)** - Design goals and philosophy
- **[Language Overview](language-overview.md)** - High-level concepts and features
- **[Standard Library Overview](standard-library-overview.md)** - Core library components

### Core Language Specification

- **[Syntax](syntax.md)** - Complete syntax reference and grammar
- **[AST](ast.md)** - Abstract Syntax Tree definition
- **[Parsing into AST](parsing-to-ast.md)** - Source code parsing process
- **[IR](ir.md)** - Intermediate Representation
- **[Compilation AST into IR](compilation-ast-ir.md)** - AST to IR transformation
- **[Environment](environment.md)** - Dynamic environment management
- **[Module System](module-system.md)** - Module loading and organization

### Evaluation System

The evaluation system is detailed in the [evaluation/](evaluation/) folder:

- **[README](evaluation/README.md)** - Evaluation overview
- **[Evaluation State](evaluation/evaluation-state.md)** - State structure
- **[Environment Preparation](evaluation/envirenment-preparation.md)** - Setup steps
- **[Module Registration](evaluation/module-registration.md)** - Module setup

The [evaluation/patterns/](evaluation/patterns/) folder contains detailed evaluation rules:

- **[README](evaluation/patterns/README.md)** - Pattern evaluation overview
- **[Literal](evaluation/patterns/literal.md)** - Literal value evaluation
- **[List](evaluation/patterns/list.md)** - List evaluation
- **[Object](evaluation/patterns/object.md)** - Object evaluation
- **[Closure](evaluation/patterns/closure.md)** - Closure evaluation
- **[Native](evaluation/patterns/native.md)** - Native function evaluation
- **[Module](evaluation/patterns/module.md)** - Module evaluation

### Standard Library

Built-in functions and types are documented in the [standard-library/](standard-library/) folder:

- **[README](standard-library/README.md)** - Library overview
- **[Bool](standard-library/bool.md)** - Boolean operations and control flow

The [standard-library/builtint/](standard-library/builtint/) folder contains built-in special forms:

- **[README](standard-library/builtint/readme.md)** - Built-in functions overview
- **[Def](standard-library/builtint/def.md)** - Variable definition
- **[Set](standard-library/builtint/set.md)** - Variable mutation
- **[Lambda](standard-library/builtint/lambda.md)** - Function creation
- **[Quote](standard-library/builtint/quote.md)** - Data literals
- **[Import](standard-library/builtint/import.md)** - Module loading

The [standard-library/math/](standard-library/math/) folder contains mathematical operations:

- **[README](standard-library/math/README.md)** - Math operations overview
- **[Arithmetic](standard-library/math/arithmetic.md)** - Arithmetic operators

## Implementation Notes

This specification is designed to be host-language agnostic, providing all necessary information for implementing Reactor in any programming environment. Data type definitions are included where essential for clarity, but implementation details are left to individual implementations.

## Development Resources

- **[Specification Development Guidelines](spec-development-guidelines.md)** - Complete rules for creating and maintaining specification content
- **[Specification Creation Plan](spec-creation-plan.md)** - Detailed plan for developing the specification

## Contributing

This specification is maintained as part of the Reactor project. For questions or contributions, please refer to the main project repository.
