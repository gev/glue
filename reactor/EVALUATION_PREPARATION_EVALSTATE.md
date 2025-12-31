# EvalState Preparation

EvalState preparation establishes the complete initial state for Reactor's evaluation system. The EvalState contains all the components needed to execute Reactor programs safely and efficiently.

## EvalState Structure

The initial EvalState is constructed with these components:

```haskell
data EvalState = EvalState
    { env :: Env           -- Current environment (initial bindings)
    , context :: Context   -- Call stack (initially empty)
    , registry :: ModuleRegistry Eval    -- Module metadata (initially empty)
    , importCache :: ImportedModuleCache Eval  -- Cached imports (initially empty)
    , rootEnv :: Env       -- Preserved root environment
    }
```

## Initial EvalState Construction

### Environment (env)
The starting environment containing initial variable bindings. This can be:
- **Standard environment**: Includes complete builtins and standard library
- **Custom environment**: Specific bindings for specialized execution contexts
- **Minimal environment**: Only essential operations

### Context (context)
Initialized as an empty list `[]` representing the initial call stack state.

### Registry (registry)
Initialized as `emptyRegistry` - no modules registered initially.

### Import Cache (importCache)
Initialized as `Map.empty` - no modules cached initially.

### Root Environment (rootEnv)
Set to the same environment as `env`. **The primary purpose of the root environment is to serve as the initial environment for imported modules**, ensuring they are evaluated in a consistent, controlled context.

## Root Environment Concept

At the foundation of Reactor's evaluation system is the **root environment** - the original, unmodified environment preserved in EvalState. **The primary purpose of the root environment is to serve as the initial environment for imported modules**, ensuring they are evaluated in a consistent, controlled context.

This root environment:
- Contains either the pristine builtins and standard library, or a custom environment
- Remains unchanged throughout program execution
- Ensures consistent module evaluation across different contexts
- Provides the stable foundation for the "tree of evaluation environments"

## Dual-Registry Architecture

Reactor uses **two separate registries** within EvalState to manage modules and their evaluation:

### Module Registry (Static Metadata)
Stores module declarations with name, exports, and unevaluated body forms. This registry is populated during the registration phase and contains static information.

### Imported Module Cache (Runtime Results)
Caches evaluated module results for reuse. This cache is populated lazily on first import and ensures modules are evaluated only once globally.

## Tree of Evaluation Environments

The root environment serves as the base for a tree structure where each evaluation context creates its own branch:

```
Root Environment (original builtins + std lib)
├── Main Program: [user_vars, builtins_frame]
│   ├── Module A Import: [module_temp, builtins_frame]
│   ├── Module B Import: [module_temp, builtins_frame]
│   └── Function Call: [local_vars, user_vars, builtins_frame]
└── REPL Session: [repl_vars, builtins_frame]
```

### Key Properties
- **Root Preservation**: Original environment never modified
- **Branch Isolation**: Each context gets its own evaluation stack
- **Shared Builtins**: Common builtin frame shared across all branches
- **Clean Merging**: Exported symbols integrate into importing scope

## Lazy Evaluation with Caching

**Registration Phase** (Eager):
- Parse module declarations
- Store metadata in Module Registry
- No evaluation of module body

**Import Phase** (Lazy with Caching):
- **First import**: Evaluate module using root environment, cache results
- **Subsequent imports**: Return cached results directly

This ensures modules are evaluated **once globally** while maintaining evaluation isolation.

## Environment Structure

Environments within EvalState are organized as stacks of frames:

- **Root Frame**: Contains builtin functions and constants (shared)
- **Module Frames**: Contain imported module bindings
- **Local Frames**: Contain function parameters and local variables
- **Temporary Frames**: Used during module evaluation (isolated)

## Security and Isolation

The root environment approach provides security benefits:
- Modules cannot access variables from importing scope during evaluation
- Clean separation between module internals and external state
- Consistent evaluation context regardless of current environment state

## Performance Characteristics

- **Memory Efficient**: Shared builtins, cached exports, temporary frame cleanup
- **Lookup Performance**: O(depth) for variable lookups, O(1) for cached imports
- **Lazy Loading**: Unused modules never evaluated
- **Global Sharing**: One evaluation serves all importers

## See Also

- [EvalState Data Structure](EVALSTATE.md) - Detailed EvalState component descriptions
- [Module Environment Architecture](MODULE_ENVIRONMENTS.md) - Detailed implementation and architecture
- [Module System Specification](MODULE_SYSTEM.md) - Complete feature overview and examples
- [Module Registration](EVALUATION_PREPARATION_MODULE_REGISTRATION.md) - Module registration process
