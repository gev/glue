# Module Environment Architecture

## Overview

This document describes Glue's **dual-registry module system** with lazy evaluation and caching. The system uses separate registries for module registration (metadata) and imported modules (cached evaluation results), ensuring modules are evaluated once and cached globally.

**📚 Related Documentation:**
- [Module System Specification](MODULE_SYSTEM.md) - Complete feature overview and examples
- [Implementation TODO](MODULE_SYSTEM_TODO.md) - Current implementation progress and roadmap

## Dual-Registry Architecture

Glue uses **two separate registries** to manage the module lifecycle:

### 1. Module Registry (Registration)
```haskell
type ModuleRegistry m = Map Text (Module m)

data Module m = Module
    { name :: Text
    , exports :: [Text]
    , body :: [IR m]  -- IR forms for evaluation
    }
```
- **Purpose**: Stores module metadata from registration phase
- **Population**: Filled during module declaration evaluation
- **Content**: Static metadata, never modified after registration

### 2. Imported Module Cache (Runtime)
```haskell
type ImportedModuleCache m = Map Text (ImportedModule m)

data ImportedModule m = ImportedModule
    { moduleName :: Text
    , exportedValues :: Map Text (IR m)  -- Cached exports
    , evaluationRootEnv :: Env m         -- Root env used for evaluation
    }
```
- **Purpose**: Caches evaluated module results for reuse
- **Population**: Filled lazily on first import
- **Content**: Runtime evaluation results + evaluation context

### Lazy Evaluation with Caching

**Registration Phase** (Eager):
- Parse module declarations
- Store metadata in Module Registry
- No evaluation of module body

**Import Phase** (Lazy with Caching):
- **First import**: Evaluate module body, cache results in Imported Cache
- **Subsequent imports**: Return cached results directly

This ensures modules are evaluated **once globally**, matching JavaScript's behavior while avoiding unnecessary computation for unused modules.

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

**Implementation: Check Cache First, Evaluate if Needed**
```haskell
importModule :: ModuleName -> Eval ()
importModule name = do
    -- Check if module already imported (cached)
    cached <- lookupImportedCache name
    case cached of
        Just imported -> do
            -- Use cached results
            mergeExportsIntoCurrent (exportedValues imported)
        Nothing -> do
            -- First import: evaluate and cache
            rootEnv <- getRootEnv

            -- Create isolated environment for evaluation
            let builtinsFrame = last rootEnv
            let isolatedEnv = pushFrame [builtinsFrame]

            -- Evaluate module in isolation
            withIsolatedEnv isolatedEnv $ do
                mod <- lookupModule name
                forM_ (body mod) eval

            -- Extract and cache exported values
            exportedValues <- extractExports isolatedEnv (exports mod)
            cacheImportedModule name exportedValues rootEnv

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
- **Builtins shared** across all modules (not duplicated)
- **Cached exports** stored globally in Imported Cache
- **Temporary frames** cleaned up after first evaluation
- **Per-import environments** created but reused from cache

#### Lookup Performance
- **First import**: O(evaluation_cost) - module body execution
- **Subsequent imports**: O(1) - direct cache lookup
- **Variable lookups**: O(depth) where depth is stack height
- **Shared builtins** reduce average lookup time
- **Cache-friendly** for frequently accessed modules

#### Caching Benefits
- **Global sharing**: One evaluation serves all importers
- **Lazy loading**: Unused modules never evaluated
- **Consistent performance**: Predictable import costs after first use

### Implementation Considerations

#### Root Environment Access
```haskell
-- Eval monad needs to track root environment separately
data Runtime = Runtime
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
