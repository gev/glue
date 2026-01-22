# Development Rules for Glue

This document outlines the most important rules and guidelines for developing and maintaining all aspects of the Glue programming language, including:

- Glue language specification
- Glue implementation
- Glue library implementation
- Glue UI framework
- Glue UI implementations
- Glue anything else

These rules apply to any development actions across the entire Glue ecosystem.

## Prerequisites

### Required Reading

Before contributing to Glue development, you must read and understand the following documents to ensure consistent understanding of Glue's unique features and proper documentation practices:

- **[Glue Language Syntax Specification](../spec/syntax.md)** - Read the complete syntax specification and EBNF grammar to understand Glue's Lisp-inspired syntax and avoid confusion with other LISP dialects like Scheme and Clojure
- **[Abstract Syntax Tree (AST) Specification](../spec/ast.md)** - Understand the AST data structure and how source code is represented after parsing
- **[Intermediate Representation (IR) Specification](../spec/ir.md)** - Understand the IR structure used for execution and compilation
- **[Markdown Style Guide for Glue Specifications](../context/markdown-style-guide.md)** - Read the rules for consistent markdown formatting, syntax highlighting, and documentation standards

## Core Development Principles

### Process Discipline
- **Do one point in time!** - Focus on single, well-defined tasks
- **Step by step!** - Break complex work into manageable, sequential steps
- **One point – one task – one commit!** - Each commit addresses exactly one logical change

### Communication and Coordination
- **Don't do anything until I ask!** - Wait for explicit direction before proceeding
- **Don't plan anything till not asked!** - Only create plans for explicitly requested tasks
- **No unsolicited initiative!** - Only execute explicit commands, no proactive suggestions or actions
- **Get free to use emoji ;)** - Emojis are encouraged for clarity and personality

### Implementation Fidelity
- **Implementations should be equal and follow uniform spec!** - All language implementations must match the Haskell reference exactly
- **Haskell implementation is the reference!** - Haskell code defines the canonical behavior for all features
- **Match Haskell AST, parser, evaluator, etc. precisely!** - No deviations allowed without explicit spec updates
- **Test against Haskell behavior!** - Ensure identical input/output for all test cases

### Quality Assurance
- **Never touch my fixes!** - Respect and preserve existing corrections
- **Read user fixes before making commits!** - Always review manual changes before committing
- **Never push to upstream!** - Local commits only, explicit push control required

### Version Control
- **One point – one task – one commit!** - Each commit addresses exactly one logical change
- **Simple commit messages!** - Keep commit messages concise and avoid shell-breaking quotes
- **Moderate descriptions** - Add brief context but avoid overly verbose explanations

## Application

These rules apply to all specification development activities:

- Document creation and modification
- Code example updates
- Cross-reference management
- Quality assurance and review
- Version control practices

## Coding Preferences

### Module Organization

Organize classes/types/functions in modules in the following order from top to bottom:

- Type declarations
- Instances
- Main API module functions
- All other API functions
- Private functions

### Haskell Code

- Use GHC 2024 features
- Actively use the following coding features enabled by the configured extensions:
  - **Block arguments**: Use do-blocks and lambda blocks as function arguments for cleaner syntax
  - **Default signatures**: Provide default type signatures for type class methods
  - **Duplicate record fields**: Define records with overlapping field names across modules
  - **Overloaded record dot**: Access record fields using dot notation with overloading
  - **Overloaded strings**: Work with string literals that can be polymorphic
  - **Record wildcards**: Use wildcards in record patterns and updates for conciseness
  - **Lambda case**: Pattern match directly in lambda expressions
  - **Multi-path if**: Use if expressions with multiple conditional branches

### Dart Code

- Use package style imports
- Prefer pattern matching in most cases
- Avoid using `dynamic` and `Object` types - prefer specific types for type safety
- **Workspace dependencies**: Use simple dependency declarations without path for workspace packages
  ```yaml
  dependencies:
    glue:        # No path needed for workspace packages
  ```
- **Package installation**: Use `flutter pub add` for Flutter packages, `dart pub add` for pure Dart packages
  ```bash
  flutter pub add code_forge      # Flutter packages
  ```

### Flutter Code

#### Code Separation by Modules (Avoid BBOM - Big Ball of Mud)

Organize code within Flutter modules to avoid monolithic classes and maintain clean separation of concerns:

- **Widget Classes**: UI component definitions and layouts
- **Business Logic**: State management, calculations, and data processing
- **Service Calls**: External API calls, database operations, and I/O
- **Utility Functions**: Helper methods, formatters, and shared logic
- **Constants**: App-wide constants, colors, strings, and configuration

#### Widget Tree Separation

- **One Widget Per Class**: Each UI component should be its own widget class
- **Stateless vs Stateful**: Use StatelessWidget for pure UI, StatefulWidget only when internal state is needed
- **Composition over Inheritance**: Build complex UIs through widget composition, not deep inheritance
- **Single Responsibility**: Each widget should have one clear purpose

#### Module Organization Order

Within each module file, organize in this order from top to bottom:

- Constants and typedefs
- Widget classes (StatelessWidget then StatefulWidget)
- Helper classes and mixins
- Utility functions
- Private implementation details
- 
- **Workspace dependencies**: Use simple dependency declarations without path for workspace packages
  ```yaml
  dependencies:
    glue_flutter: # Workspace resolution handles local development
  ```
- **Package installation**: Use `flutter pub add` for Flutter packages, `dart pub add` for pure Dart packages
  ```bash
  flutter pub add code_forge      # Flutter packages
  ```


#### State Management

- **Widget Lifecycle**: Properly dispose controllers and clean up resources

#### Naming Conventions

- **Widgets**: PascalCase (MyCustomWidget)
- **Variables**: camelCase (myVariableName)
- **Files**: snake_case (custom_widget.dart)
- **Folders**: snake_case (ui_components/)

### Cross-Language Consistency

Same code and tests in different languages should have the same structure:

- Same number of sense items
- Same documentation
- Order in modules
- Naming (with adjustments for language features)

## Rationale

These rules ensure:
- **Consistency** - Uniform approach across all contributors
- **Quality** - Careful, deliberate progress with proper review
- **Coordination** - Clear communication and task boundaries
- **Accountability** - Explicit approval and change tracking
