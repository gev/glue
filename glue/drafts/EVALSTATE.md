# Glue EvalState Data Structure

## Overview

EvalState represents the complete state of Glue's evaluation system, containing all information needed to execute Glue programs.

## Core Data Types

### EvalState

```haskell
data EvalState = EvalState
    { env :: Env
    , context :: Context
    , registry :: ModuleRegistry Eval
    , importCache :: ImportedModuleCache Eval
    , rootEnv :: Env
    }
```

## EvalState Components

### Environment (env)
- **Type:** `Env`
- **Purpose:** Current variable bindings during evaluation
- **Structure:** Stack of frames containing symbol-to-value mappings

### Context (context)
- **Type:** `Context`
- **Purpose:** Call stack for error reporting
- **Structure:** List of function names showing execution path

### Registry (registry)
- **Type:** `ModuleRegistry Eval`
- **Purpose:** Stores registered module metadata
- **Structure:** Map from module names to module definitions

### Import Cache (importCache)
- **Type:** `ImportedModuleCache Eval`
- **Purpose:** Caches evaluated module results
- **Structure:** Map from module names to imported module data

### Root Environment (rootEnv)
- **Type:** `Env`
- **Purpose:** Original environment passed to evaluation
- **Structure:** Preserved initial environment state

## Summary

EvalState encapsulates the complete runtime state of Glue's evaluation system:

- **Environment management** for variable scoping
- **Call stack tracking** for error reporting
- **Module system state** for imports and exports
- **Caching infrastructure** for performance optimization
- **Root environment preservation** for consistent evaluation
