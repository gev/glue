# Module Environment Architecture

## Overview

This document describes the **root environment approach** for Reactor's module system, where each imported module gets its own evaluation environment derived from the original environment passed to `runEval`.

**📚 Related Documentation:**
- [Module System Specification](MODULE_SYSTEM.md) - Complete feature overview and examples
- [Implementation TODO](MODULE_SYSTEM_TODO.md) - Current implementation progress and roadmap

## Environment Structure

```haskell
type Env m = [Frame m]        -- Stack of frames
type Frame m = Map Text (IR m) -- Single frame with bindings
```

Environments are stacks of frames, searched from top to bottom for variable lookups.

## Root Environment Approach

The system maintains a **tree of evaluation environments** where each module import creates a new branch from the root environment.

```
Root Environment (original passed to runEval)
├── Main Program: [user_vars, builtins_frame]
│   ├── Module A Import: [module_temp, builtins_frame]
│   ├── Module B Import: [module_temp, builtins_frame]
│   └── Function Call: [local_vars, user_vars, builtins_frame]
└── REPL Session: [repl_vars, builtins_frame]
```

### Key Properties

- **Root Preservation**: Original environment never modified
- **Branch Isolation**: Each evaluation context gets its own stack
- **Shared Builtins**: Common builtin frame shared across all contexts
- **Clean Merging**: Exported symbols integrate into importing scope

### Implementation Details

#### During Module Import

**Correct Implementation: Use Root Environment**
```haskell
importModule :: ModuleName -> Eval ()
importModule name = do
    -- Get the root environment (original passed to runEval)
    rootEnv <- getRootEnv  -- [original_libWithModules_frame]

    -- Extract pristine builtins from root environment
    let builtinsFrame = last rootEnv  -- Always original builtins
    let isolatedEnv = pushFrame [builtinsFrame]  -- [temp_frame, original_builtins]

    -- Evaluate module in isolation
    withIsolatedEnv isolatedEnv $ do
        mod <- lookupModule name
        forM_ (body mod) eval  -- Only sees original builtins + module internals

    -- Extract exported values
    exportedValues <- extractExports isolatedEnv (exports mod)

    -- Merge into current environment
    mergeExportsIntoCurrent exportedValues
```

**Why Root Environment?**
The root environment contains the **original, unmodified** builtins and module system that were passed to `runEval`. Even if user code modifies the current environment during execution, the root environment remains pristine, ensuring modules always get consistent builtins.

#### Environment Transitions

```
Before Import:
[user_vars, builtins]

During Module Evaluation:
[temp_module_vars, builtins]  ← Isolated!

After Import:
[imported_vars, user_vars, builtins]  ← Merged
```

### Tree Structure Reality

The "tree of stacks" emerges from evaluation contexts:

```
Root: Global Builtins [builtins_frame]
├── Branch: Main Program [user_vars, builtins_frame]
│   ├── Branch: Module A Import [module_a_temp, builtins_frame]
│   ├── Branch: Module B Import [module_b_temp, builtins_frame]
│   └── Branch: Module C Import [module_c_temp, builtins_frame]
└── Branch: Function Call [local_vars, user_vars, builtins_frame]
```

Each evaluation context maintains its own stack while sharing the common builtins root.

### Security Benefits

#### Isolation During Loading
- Modules cannot access variables from importing scope
- No access to sensitive user data during module initialization
- Clean separation between module internals and external state

#### Example Security Issue Prevented

```clojure
;; Main program
(def secret-password "admin123")

;; Malicious module
(module evil
  (export steal)
  (def steal (lambda () secret-password)))  ;; Cannot access!

;; Usage
(import evil)
(steal)  ;; Returns undefined/error, not "admin123"
```

### Performance Characteristics

#### Memory Efficiency
- Builtins shared across all modules (not duplicated)
- Temporary evaluation frames cleaned up after import
- Only exported values retained in importing environment

#### Lookup Performance
- Variable lookups: O(depth) where depth is stack height
- Shared builtins reduce average lookup time
- Cache-friendly for frequently accessed builtins

### Implementation Considerations

#### Root Environment Access
```haskell
-- Eval monad needs to track root environment separately
data EvalState = EvalState
    { currentEnv :: Env Eval
    , rootEnv :: Env Eval  -- Original environment passed to runEval
    }

getRootEnv :: Eval (Env Eval)
getRootEnv = do
    -- Access the preserved root environment
    -- Implementation depends on Eval monad structure
```

#### Export Merging
```haskell
mergeExportsIntoCurrent :: [(Text, IR m)] -> Eval ()
mergeExportsIntoCurrent exports = do
    currentEnv <- getEnv
    let newFrame = Map.fromList exports
    putEnv (newFrame : currentEnv)
```

#### Isolation Helper
```haskell
withIsolatedEnv :: Env m -> Eval a -> Eval a
withIsolatedEnv isolatedEnv action = do
    originalEnv <- getEnv
    putEnv isolatedEnv
    result <- action
    putEnv originalEnv
    pure result
```

## Comparison with Other Languages

| Language | Environment Structure | Isolation Level |
|----------|----------------------|-----------------|
| Reactor | Shared builtins, isolated stacks | High (module-level) |
| Python | Global module dicts | Medium (module-level) |
| JavaScript | Prototype chains | Low (property access) |
| Common Lisp | Dynamic environments | High (lexical + dynamic) |

## Future Extensions

### Qualified Imports
```clojure
(import math.utils as m)  ;; Creates namespace frame
```
Would add namespace frames to the environment stack.

### Nested Module Evaluation
Modules that import other modules would create deeper evaluation trees.

### Hot Reloading
Environment snapshots could enable module hot reloading with proper isolation.

## Conclusion

The recommended approach provides the best balance of:
- **Security** through evaluation isolation
- **Efficiency** through builtin sharing
- **Clean architecture** with proper scoping
- **Extensibility** for future features

The "tree of stacks" structure emerges naturally from the evaluation model while maintaining the simplicity and performance of stack-based environments.
