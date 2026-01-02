# 🚀 Reactor Language Specification

Complete specification for the Reactor programming language, a Lisp-inspired language with modern enhancements for property objects, functional programming, and module systems.

## 🎯 Overview

Reactor is designed to be:
- **Simple**: Minimal syntax with maximal expressiveness
- **Safe**: Prevents common programming errors through design
- **Flexible**: Easy to extend and embed in host applications
- **Modern**: Combines Lisp elegance with contemporary features

### Introduction
- **[Language Aims](language-aims.md)** - Design goals and philosophy
- **[Language Overview](language-overview.md)** - High-level introduction
- **[Execution Pipeline](execution-pipeline.md)** - How Reactor processes code

## Specification Structure

### Core Language
- **[Syntax](syntax.md)** - Complete syntax reference and grammar
- **[AST](ast.md)** - Abstract Syntax Tree representation
- **[Parsing](parsing-to-ast.md)** - Source to AST conversion
- **[IR](ir.md)** - Intermediate Representation for execution
- **[Compilation](compilation-ast-ir.md)** - AST to IR transformation

### Runtime System
- **[Environment](environment.md)** - Variable scoping and binding
- **[Module System](module-system.md)** - Code organization and imports

### Evaluation
- **[Evaluation Overview](evaluation/README.md)** - Runtime execution model
- **[Evaluation State](evaluation/evaluation-state.md)** - Execution context
- **[Evaluation Preparation](evaluation/envirenment-preparation.md)** - Setup and initialization
- **[Module Registration](evaluation/module-registration.md)** - Module loading process
- **[Evaluation Patterns](evaluation/patterns/)** - Detailed evaluation rules

### Standard Library
- **[Standard Library Overview](standard-library-overview.md)** - Library organization
- **[Built-in Functions](standard-library/builtint/)** - Core language features
- **[Bool Module](standard-library/bool.md)** - Boolean operations and control flow
- **[Math Module](standard-library/math/)** - Mathematical operations

## Development Guidelines

This specification follows strict development guidelines to ensure consistency and implementability:

1. **Host-Language Agnostic** - No reference to specific implementation languages
2. **Minimal Code Examples** - Only essential code snippets, using pseudocode when needed
3. **Technical Focus** - Emphasis on "what" and "how" for language implementers
4. **Cross-References** - Extensive linking between related documents
5. **Self-Contained** - Each document provides necessary context
6. **Consistent Terminology** - Standardized technical terms throughout

## Reading Order

For first-time readers, follow this sequence:

1. **Start Here** → [Language Overview](language-overview.md)
2. **Goals** → [Language Aims](language-aims.md)
3. **How It Works** → [Execution Pipeline](execution-pipeline.md)
4. **Foundation** → [Syntax](syntax.md)
5. **Implementation** → [AST](ast.md) → [IR](ir.md) → [Compilation](compilation-ast-ir.md)
6. **Runtime** → [Environment](environment.md) → [Evaluation Overview](evaluation/README.md)
7. **Organization** → [Module System](module-system.md)
8. **Library** → [Standard Library Overview](standard-library-overview.md)

## Implementation Status

- ✅ **Complete**: Syntax, AST, IR, Compilation, Environment, Module System
- 🚧 **In Progress**: Evaluation system, Standard Library
- 📋 **Planned**: Advanced features, error handling, optimization

## Contributing

This specification is automatically generated from implementation documentation. To contribute:

1. Modify the source documentation in `reactor/drafts/`
2. Follow the [development guidelines](spec-development-guidelines.md)
3. Ensure cross-references are updated
4. Test examples in a Reactor implementation

## License

This specification is part of the Reactor project. See project LICENSE for details.
