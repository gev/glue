# Development Rules for Glue

This document outlines the most important rules and guidelines for developing and maintaining all aspects of the Glue programming language, including:

- Glue language specification
- Glue implementation
- Glue library implementation
- Glue UI framework
- Glue UI implementations
- Glue anything else

These rules apply to any development actions across the entire Glue ecosystem.

## Core Development Principles

### Process Discipline
- **Do one point in time!** - Focus on single, well-defined tasks
- **Step by step!** - Break complex work into manageable, sequential steps
- **One point – one task – one commit!** - Each commit addresses exactly one logical change

### Communication and Coordination
- **Don't do anything until I ask!** - Wait for explicit direction before proceeding
- **Don't plan anything till not asked!** - Only create plans for explicitly requested tasks
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

## Application

These rules apply to all specification development activities:

- Document creation and modification
- Code example updates
- Cross-reference management
- Quality assurance and review
- Version control practices

## Rationale

These rules ensure:
- **Consistency** - Uniform approach across all contributors
- **Quality** - Careful, deliberate progress with proper review
- **Coordination** - Clear communication and task boundaries
- **Accountability** - Explicit approval and change tracking
