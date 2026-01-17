# Partial Application Implementation Plan

## Overview
Implement partial application (currying) support for native functions to make Glue a proper functional programming language with first-class functions.

## Current Issue
`NativeFunc` doesn't support partial application while `Closure` does, breaking functional programming principles.

## Solution: Universal Currying Contract
- **Single contract for ALL functions:** `IR → IR`
- Functions take one argument, return result or closure
- Automatic partial application (Haskell-style currying)
- No arity declarations or complex logic

## Implementation Phases

### Phase 1: Constructor Migration (Steps 1-3)
**Goal:** Move constructors from ModuleInfo to function implementations

#### Haskell Implementation
1. Move `NativeFunc` AND `Special` constructors from every ModuleInfo declarations to function implementations (run tests and make commit for every ModuleInfo)
2. Check the all Libraries (ModuleInfo) !
3. Check the tests

#### Dart Implementation
Make changes in the Dart implementation in the same order from 1 to 3

**See:** [Development Technology](development-technology.md) for cross-language synchronization requirements

### Phase 2: Currying Implementation (Steps 4-12)
**Goal:** Change NativeFunc to single-argument contract and implement currying

#### Haskell Implementation
4. Change `NativeFunc` from `[IR] → IR` to `IR → IR`
5. Rewrite all native functions to use currying internally
6. Update evaluator for single-argument application
7. Run tests
8. Commit
9. Add currying tests into `EvalSpec.hs`
10. Run tests
11. Fix bugs
12. Commit

#### Dart Implementation
Make changes in the Dart implementation in the same order from 4 to 12

**See:** [Development Technology](development-technology.md) for cross-language synchronization requirements

### Documentation
Update drafts and specifications
Make commit

**See:** [Implementation Verification](implementation-verification.md) for testing and validation procedures

## Key Technical Decisions

- **Universal contract**: All functions `IR → IR` (single argument)
- **Internal currying**: Functions decide when to return result vs closure
- **Automatic partial application**: Every call can be partial
- **Special forms**: No partial application (syntactic constructs)
- **Pure functional**: Haskell-style evaluation model

## Success Criteria

- ✅ `((+ 1) 2)` returns `3`
- ✅ `((cons 1) (2 3 4))` works for lists
- ✅ `((print "hello") "world")` prints both strings
- ✅ Performance: Simple single-argument application
- ✅ Pure functional: Haskell-style currying throughout
- ✅ Cross-implementation: Haskell and Dart behave identically

## Rationale

This approach:
- Maintains functional programming principles
- Provides zero-overhead partial application
- Reuses existing infrastructure
- Enables code like `((+ 1) 2)` and `((map f) list)`
- Works for both positional and named argument functions
