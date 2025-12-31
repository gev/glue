# Reactor EvalState Data Structure

## Overview

EvalState represents the complete state of Reactor's evaluation system, containing all information needed to execute Reactor programs.

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

### Environment Types

```haskell
type Env m = [Frame m]
type Frame m = Map Text (IR m)
type Context = [Text]
```

### Module System Types

```haskell
type ModuleRegistry m = Map Text (Module m)
type ImportedModuleCache m = Map Text (ImportedModule m)

data Module m = Module
    { name :: Text
    , exports :: [Text]
    , body :: [IR m]
    }

data ImportedModule m = ImportedModule
    { moduleName :: Text
    , exportedValues :: Map Text (IR m)
    , evaluationRootEnv :: Env m
    }
```

### IR Types

```haskell
data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Module (Map Text (IR m))
    | Native (Native m)
    | Closure [Text] (IR m) (Env m)

data Native m
    = Func ([IR m] -> m (IR m))
    | Cmd ([IR m] -> m ())
    | Special ([IR m] -> m (Maybe (IR m)))
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

EvalState encapsulates the complete runtime state of Reactor's evaluation system:

- **Environment management** for variable scoping
- **Call stack tracking** for error reporting
- **Module system state** for imports and exports
- **Caching infrastructure** for performance optimization
- **Root environment preservation** for consistent evaluation
