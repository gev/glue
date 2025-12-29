# Module Environment Architecture

## Overview

This document describes the environment structure and isolation strategy for Reactor's module system, addressing the question: "Should each imported module have its own environment?"

**📚 Related Documentation:**
- [Module System Specification](MODULE_SYSTEM.md) - Complete feature overview and examples
- [Implementation TODO](MODULE_SYSTEM_TODO.md) - Current implementation progress and roadmap

## Current Environment Structure

```haskell
type Env m = [Frame m]        -- Stack of frames
type Frame m = Map Text (IR m) -- Single frame with bindings
```

Environments are stacks of frames, searched from top to bottom for variable lookups.

## Environment Structure Options

### Option 1: Each Module Gets Its Own Complete Env Stack

```
Main Program Env: [user_vars, builtins]
├── Module A Env: [module_a_vars, builtins]
├── Module B Env: [module_b_vars, builtins]
└── Module C Env: [module_c_vars, builtins]
```

**Characteristics:**
- Each module has a complete, independent environment stack
- Redundant builtin frames across modules
- No sharing of imported modules
- Complex tree management and memory usage

**Problems:**
- Memory inefficient (builtins duplicated)
- No sharing of transitively imported modules
- Complex ownership and lifecycle management

### Option 2: Shared Builtins, Isolated Evaluation Stacks ⭐ (Recommended)

```
Global Builtins: [builtins_frame] (shared)

Main Program: [user_vars, builtins_frame]
├── Module A Evaluation: [module_a_temp_vars, builtins_frame]
├── Module B Evaluation: [module_b_temp_vars, builtins_frame]
└── Module C Evaluation: [module_c_temp_vars, builtins_frame]

After Import: [imported_symbols, user_vars, builtins_frame]
```

**Characteristics:**
- Builtins shared across all contexts
- Each module evaluation gets isolated stack
- Exported values merge into importing environment
- Tree-like structure emerges naturally

### Option 3: Flat Global Environment

```
Single Global Env: [all_modules, all_user_vars, builtins]
```

**Problems:**
- No isolation during module loading
- Security vulnerabilities (modules can access user code)
- Naming conflicts between modules
- No proper scoping

## Recommended Approach: Option 2

### Why This Structure?

**Security:** Modules cannot access user code during loading
**Efficiency:** Builtins shared, not duplicated
**Isolation:** Each evaluation context is separate
**Clean Merging:** Exported values integrate seamlessly

### Implementation Details

#### During Module Import

**Correct Implementation: Fresh Environment with Static Builtins**
```haskell
importModule :: ModuleName -> Eval ()
importModule name = do
    -- Create isolated environment with static builtins only
    let isolatedEnv = pushFrame (fromFrame Lib.lib)  -- [temp_frame, builtins]

    -- Evaluate module in isolation
    withIsolatedEnv isolatedEnv $ do
        mod <- lookupModule name
        forM_ (body mod) eval  -- Only sees builtins + module internals

    -- Extract exported values
    exportedValues <- extractExports isolatedEnv (exports mod)

    -- Merge into current environment
    mergeExportsIntoCurrent exportedValues
```

**Why Static Builtins?**
Since modules are **eagerly registered** before any Reactor code runs, builtins are static and cannot be dynamically extended. The registration environment contains only the special forms needed for parsing, not the full runtime builtins.

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

#### Simplified Environment Creation
```haskell
createModuleEnv :: Env Eval
createModuleEnv = pushFrame (fromFrame Lib.lib)  -- [temp_frame, builtins]
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
