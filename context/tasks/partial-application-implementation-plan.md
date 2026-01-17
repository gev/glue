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

### Phase 1: Haskell Core Changes
1. **Modify IR.hs**: Add arity fields to `NativeFunc` type
   ```haskell
   | NativeFunc ([IR m] -> m (IR m)) Int (Maybe Int)  -- func, minArity, maxArity
   ```

2. **Update Eval.hs**: Modify `apply` to handle partial application
   ```haskell
   applyNativeFunc f minArity maxArity rawArgs = do
       args <- mapM eval rawArgs
       if length args < minArity then
           -- Create closure for partial application
           createPartialClosure f args (minArity - length args)
       else if length args > maxArity then
           throwError wrongNumberOfArguments
       else
           f args  -- Full application
   ```

### Phase 2: Function Updates (Incremental)
3. **Change function signatures**: From `[IR] -> Eval IR` to structured return type
4. **Remove manual arg validation**: Functions assume correct arg count
5. **Update module registrations**: Remove `NativeFunc` constructors
6. **Test each module**: Ensure functionality preserved

### Phase 3: Testing & Refinement
7. **Add comprehensive tests**: Partial application scenarios
8. **Performance validation**: Ensure no runtime overhead
9. **Edge case handling**: Error conditions and type safety

### Phase 4: Dart Implementation
10. **Repeat all steps**: Match Haskell exactly
11. **Cross-implementation testing**: Verify identical behavior

### Phase 5: Documentation
12. **Update specifications**: Document partial application behavior
13. **Update drafts**: Technical implementation details

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
1. Move NativeFunc constructors from ModuleInfo to function implementations (incremental commits)
2. Fix all libraries to match new structure
3. Check tests
4. Add arity info into NativeFunc
5. Fix all libraries
6. Run tests
7. Commit
8. Implement partial application
9. Add special tests into EvalSpec.hs
10. Run tests
11. Fix bugs
12. Commit

**Dart:**
Repeat steps 1-12

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
