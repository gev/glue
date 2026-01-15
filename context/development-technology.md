# Glue Language Development Technology

## Outer Development Cycle 🔄 (Cross-Language Synchronization)

### When Spec or Haskell Implementation Changes
1. **Update Spec** - Modify specification documents as needed
2. **Update Haskell Implementation** - Make changes to reference implementation
3. **Update Haskell Tests** - Ensure all Haskell tests pass with changes
4. **Update Other Implementations** - Propagate changes to all language implementations
5. **Update Cross-Language Tests** - Synchronize test suites across languages
6. **Verify Consistency** - Ensure all implementations behave identically
7. **Update Documentation** - Reflect changes in all relevant docs

### Key Requirements for Changes
- **Haskell Reference**: Any behavioral changes must be validated in Haskell first
- **Spec Updates**: Specification must be updated before implementation changes
- **Cross-Language Sync**: All implementations must be updated to match new behavior
- **Test Synchronization**: Test cases must be identical across languages
- **Documentation**: All changes must be documented consistently

## Core Development Process (Haskell Reference Implementation)

### 1. Make Spec 📋
- Create comprehensive language specification in `/spec/` directory
- Define syntax, AST, IR, evaluation semantics, standard library
- Use host-language agnostic descriptions
- Include examples and cross-references between documents
- Follow strict guidelines from `context/spec-development-guidelines.md`

### 2. Make Haskell Implementation 💻
- Implement in `/haskell/glue/src/` following spec exactly
- **Haskell is the reference implementation** - defines canonical behavior
- Structure: `Glue/` (core), `Glue/Lib/` (standard library), `Glue/Eval/` (evaluation)
- Use proper Haskell patterns: monads, ADTs, type classes
- Document all functions and types

### 3. Make Tests ✅
- Comprehensive HSpec test suites in `/haskell/glue/test/`
- Test every function, edge case, and integration scenario
- Include both unit tests and system integration tests
- Verify spec compliance and catch regressions
- Tests serve as executable documentation

### 4. Analyze 🔍
- Run full test suite and verify all pass
- Review code quality, performance, correctness
- Check spec completeness and accuracy
- Identify areas needing improvement

### 5. If Necessary Change the Specs 📝
- Update spec documents to reflect implementation realities
- Improve design based on implementation experience
- Maintain backward compatibility when possible
- Update cross-references and examples

### 6. Repeat 🔄
- Iterate until spec and implementation are stable
- Ensure all tests pass consistently
- Prepare for other language implementations

## Translation to Other Languages 🌍

### 1. Use Spec 📖
- Reference `/spec/` documents as authoritative source
- Understand syntax, semantics, and behavior requirements
- Study examples and edge cases from spec

### 2. Use Haskell Code as Reference 🎯
- **Haskell implementation is the gold standard**
- Match input/output behavior exactly
- Study Haskell code structure and algorithms
- Test against Haskell behavior for correctness

### 3. Code Structure Should Repeat Haskell Code Structure 📁
- Mirror Haskell module organization
- Same package/directory layout
- Equivalent file naming and grouping
- Parallel type definitions and function organization

### 4. Tests Structure Should Repeat Haskell Tests Structure 🧪
- **Exact structural mirroring**: Directory hierarchies must match Haskell exactly
- **One-to-one file mapping**: Every Haskell test file must have equivalent target file
- **Same directory organization**: Identical folder structures for test modules
- **File naming convention**: `FunctionSpec.hs` → `function_test.ext`
- **Module correspondence**: Test modules mirror source module structure exactly
- **Same test cases**: Identical test scenarios with language-appropriate syntax
- **Match coverage exactly**: Every Haskell test must be mirrored
- **Testing frameworks**: Use equivalent frameworks (HSpec → test package, etc.)

### 5. Naming in Language Should be Equivalent to Haskell 🏷️
- Translate Haskell function names appropriately
- Maintain semantic equivalence
- Use language conventions while preserving meaning
- Example: `evalBody` → `evalBody`, `isCallable` → `isCallable`

### 6. Order of Types, Functions Should be the Same 📋
- Maintain declaration order from Haskell
- Same grouping of related functionality
- Parallel module structure and exports
- Consistent organization across implementations

### 7. Code Docs Should be the Same 📚
- Copy Haskell documentation comments
- Adapt to language documentation conventions
- Preserve technical accuracy and examples
- Maintain consistency across implementations

## Key Principles 🎯

- **Haskell Reference**: All implementations must match Haskell exactly
- **Spec First**: Specification drives all implementations
- **Test Driven**: Comprehensive tests ensure correctness
- **Iterative**: Continuous improvement through analysis
- **Consistent**: Same structure, naming, and behavior across languages
- **Documented**: Clear documentation at all levels

This process ensures multiple correct implementations of Glue that behave identically while being maintainable and extensible. 🚀
