# Reactor EvalState Data Structure

## Overview

EvalState represents the complete state of Reactor's evaluation system, containing all information needed to execute Reactor programs.

## Core Data Types

### EvalState

```haskell
data EvalState = EvalState
    { env :: Env
    , context :: Context
    , rootEnv :: Env
    }
```

### Environment Types

```haskell
type Env m = [Frame m]
type Frame m = Map Text (IR m)
type Context = [Text]
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

### Root Environment (rootEnv)
- **Type:** `Env`
- **Purpose:** Original environment passed to evaluation
- **Structure:** Preserved initial environment state

## Summary

EvalState encapsulates the complete runtime state of Reactor's evaluation system:

- **Environment management** for variable scoping
- **Call stack tracking** for error reporting
- **Root environment preservation** for consistent evaluation
