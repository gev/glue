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

## Module Overview

### Haskell Implementation
- **Bool Module**: 11 functions (eq, ne, lt, le, gt, ge, not, if, when, while, until)
- **IO Module**: 3 functions (print, println, read-line)
- **Builtin Module**: 7+ functions (def, set, lambda, let, import, try, error, etc.)
- **List Module**: 21 functions (append, butlast, car, cdr, cons, drop, filter, find, flatten, last, length, map, member, nth, partition, position, remove, reverse, sort, take, zip)
- **Math Module**: 25+ functions across 5 submodules (Arithmetic, Power, Trigonometric, Logarithmic, Utility)

### Dart Implementation
- **Bool Module**: 11 functions (identical to Haskell)
- **IO Module**: 3 functions (identical to Haskell)
- **Builtin Module**: 7+ functions (identical to Haskell)
- **List Module**: 21 functions (identical to Haskell)
- **Math Module**: 25+ functions (identical to Haskell)

**Total: 140+ files across both implementations**

**See detailed file inventories in the phase-specific plans below.**

## Implementation Phases

### Phase 1: Constructor Migration
**Goal:** Move constructors from ModuleInfo to function implementations

**See:** [Partial Application Phase 1 Plan](partial-application-phase1-plan.md) for detailed implementation steps

### Phase 2: Currying Implementation
**Goal:** Change NativeFunc to single-argument contract and implement currying

**See:** [Partial Application Phase 2 Plan](partial-application-phase2-plan.md) for detailed implementation steps

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
