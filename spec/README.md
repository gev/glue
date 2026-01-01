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

## Specification Development Guidelines

These rules guide the creation and maintenance of this specification:

### Content Adaptation Rules

1. **Host-Language Agnostic**: All content will be written without reference to specific implementation languages (no Haskell, Dart, or TypeScript specifics except where FFI examples are needed).

2. **Minimal Code Examples**: Only include Haskell data type definitions when absolutely essential for clarity. Use pseudocode or plain English descriptions for algorithms and processes.

3. **Source Material**: Extract and adapt content from the existing `reactor/*.md` documentation files, synthesizing information from multiple sources to create coherent specification chapters.

4. **Structure Compliance**: Follow the exact folder and file structure created, with each folder containing a README.md introduction.

### Writing Guidelines

5. **Specification Style**: Write in clear, technical language suitable for language implementers. Focus on "what" and "how" rather than "why" (which goes in language-aims.md).

6. **Syntax Examples**: Use Reactor code examples liberally, formatted with proper syntax highlighting.

7. **Cross-References**: Include links between related documents using relative paths.

8. **Completeness**: Cover all essential aspects without unnecessary implementation details.

### Content Organization Rules

9. **Chapter Splitting**: Long evaluation and standard library chapters are split into logical sub-documents grouped in folders.

10. **README Files**: Every folder gets an introductory README that outlines the contents and provides navigation.

11. **Consistent Terminology**: Use consistent technical terms throughout (e.g., "property object" not "dict", "atom" for primitive values).

### Quality Assurance Rules

12. **Accuracy**: Ensure all technical details match the existing Reactor implementation documentation.

13. **Clarity**: Prefer simple explanations over complex ones, with examples to illustrate concepts.

14. **Completeness**: Each document should be self-contained with necessary context, while referencing related documents for deeper details.

## Specification Creation Plan

This section outlines the detailed process for creating and maintaining the Reactor language specification:

### Phase 1: Research and Preparation

1. **Complete Documentation Review**
   - Read all reactor/*.md files thoroughly
   - Extract key concepts, syntax rules, and implementation details
   - Identify relationships between documents
   - Note Haskell-specific code that needs to be made language-agnostic

2. **Content Mapping**
   - Map existing reactor docs to specification chapters
   - Identify gaps in current documentation
   - Plan cross-references between documents
   - Create content outline for each document

3. **Guidelines Application**
   - Review the 14 specification development rules
   - Ensure all content follows host-language agnostic approach
   - Plan how to convert Haskell examples to pseudocode/plain English

### Phase 2: Document Creation Workflow

For each document, follow this process:

4. **Document Planning**
   - Define document scope and purpose
   - List key sections and subsections
   - Identify source material from reactor docs
   - Plan examples and code snippets

5. **Content Synthesis**
   - Extract relevant information from source docs
   - Rewrite in specification style (technical, implementation-focused)
   - Remove Haskell-specific code and replace with generic descriptions
   - Ensure consistent terminology throughout

6. **Structure and Organization**
   - Write introduction/overview section
   - Organize content logically (simple to complex)
   - Add cross-references to related documents
   - Include examples with Reactor syntax

7. **Quality Assurance**
   - Check adherence to development guidelines
   - Verify completeness and accuracy
   - Ensure self-contained content with proper context
   - Review for clarity and technical precision

### Phase 3: Integration and Review

8. **Cross-Document Integration**
   - Update all cross-references between documents
   - Ensure consistent linking and navigation
   - Verify that README structure matches actual files

9. **Comprehensive Review**
   - Read through entire specification for consistency
   - Check that all referenced documents exist
   - Validate that examples work and are correct
   - Ensure no broken links or missing information

10. **Final Polish**
    - Update any outdated references
    - Add any missing overview or summary sections
    - Final proofreading for clarity and accuracy
    - Prepare for implementation team review

### Implementation Order

**Priority Order for Document Creation:**
1. Syntax (foundation for all other docs)
2. AST (needed for parsing and compilation docs)
3. IR (needed for compilation doc)
4. Parsing (depends on AST)
5. Compilation (depends on AST and IR)
6. Environment (core runtime concept)
7. Module System (depends on environment)
8. Evaluation State (foundation for evaluation docs)
9. Evaluation Preparation (depends on state)
10. Evaluation Patterns (detailed evaluation rules)
11. Standard Library Overview (high-level view)
12. Built-in Functions (core language features)
13. Bool and Math (specific library components)

## Contributing
