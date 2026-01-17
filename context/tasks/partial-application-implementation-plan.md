# Partial Application Implementation Plan

## Overview
Implement partial application (currying) support for native functions to make Glue a proper functional programming language with first-class functions.

## Current Issue
`NativeFunc` doesn't support partial application while `Closure` does, breaking functional programming principles.

## Solution: Hybrid Approach
- Add arity metadata (`minArity`, `maxArity`) to `NativeFunc` IR type
- Partial application creates `Closure` objects that wrap native function calls
- Zero runtime overhead (simple length checks)
- Reuse existing closure machinery for parameter binding

## Implementation Steps

### Haskell Implementation (Steps 1-12)
1. Move `NativeFunc` AND `Special` constructors from every ModuleInfo declarations to function implementations (run tests and make commit for every ModuleInfo)
2. Check the all Libraries (ModuleInfo) !
3. Check the tests
4. Add arity info into `NativeFunc`
5. Fix the all libraries
6. Run tests
7. Commit
8. Implement partial application
9. Add special tests into `EvalSpec.hs`
10. Run tests
11. Fix bugs
12. Commit

### Dart Implementation
Make changes in the Dart implementation in the same order from 1 to 12

**See:** [Development Technology](development-technology.md) for cross-language synchronization requirements

### Documentation
Update drafts and specifications
Make commit

**See:** [Implementation Verification](implementation-verification.md) for testing and validation procedures

## Key Technical Decisions

- **Arity representation**: `(minArity, maxArity)` where `Nothing` = unlimited
- **Partial application**: Reuses `Closure` type with synthetic parameter names
- **Named args**: Handled as single Object argument (arity 1)
- **Special forms**: No partial application (syntactic constructs)
- **Backward compatibility**: Changes are internal, API remains stable

## Success Criteria

- ✅ `((+ 1) 2)` returns `3`
- ✅ `((cons 1) (2 3 4))` works for lists
- ✅ Named functions work: `(person :name "Bob")` creates partial
- ✅ Performance: No overhead for full application
- ✅ Type safety: Proper error messages for arity mismatches
- ✅ Cross-implementation: Haskell and Dart behave identically

## Implementation Order

**Haskell:**
1. Move `NativeFunc` AND `Special` constructors from ModuleInfo declarations to function implementations (run tests and make commit for every ModuleInfo)
2. Fix the all Libraries!
3. Check the tests
4. Add arity info into `NativeFunc`
5. Fix the all libraries
6. Run tests
7. Commit
8. Implement partial application
9. Add special tests into `EvalSpec.hs`
10. Run tests
11. Fix bugs
12. Commit

**Dart:**
Make changes in the Dart implementation in the same order from 1 to 12

**Documentation:**
Update drafts and specifications
Make commit

## Rationale

This approach:
- Maintains functional programming principles
- Provides zero-overhead partial application
- Reuses existing infrastructure
- Enables code like `((+ 1) 2)` and `((map f) list)`
- Works for both positional and named argument functions
