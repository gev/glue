
# 🔄 Reactor Execution Pipeline

Reactor transforms source code through this carefully designed pipeline:

```
Source Text → Parse → AST → Compile → IR → Prepare Environment → Register Modules → Create EvalState → Evaluate → Result
```

## Pipeline Stages

### 1. Source Text
Human-readable Reactor code input as text.

### 2. Parse
Convert text to Abstract Syntax Tree (AST) using the language grammar. Validates syntax and creates structured representation.

### 3. AST (Abstract Syntax Tree)
Tree structure representing the syntactic structure of the program. Contains nodes for atoms, lists, property objects, and quoted expressions.

### 4. Compile
Transform AST into Intermediate Representation (IR). Performs semantic analysis, type checking, and optimization.

### 5. IR (Intermediate Representation)
Lower-level representation optimized for execution. Contains primitives, closures, objects, and native function calls.

### 6. Prepare Environment
Set up the execution environment with built-in functions, constants, and initial state.

### 7. Register Modules
Load and register external modules, making their exports available in the environment.

### 8. Create EvalState
Complete execution context with environment, loaded modules, and evaluation state.

### 9. Evaluate
Execute the IR using the evaluation semantics, applying functions, evaluating conditionals, and performing computations.

### 10. Result
Final computed value or side effects produced by the program execution.

## Key Benefits

- **Safety**: Each stage validates and transforms safely
- **Performance**: Optimized IR enables efficient execution
- **Modularity**: Clear separation of concerns
- **Debuggability**: Pipeline stages aid error diagnosis

## Related Documents

- [Language Overview](language-overview.md) - High-level introduction
- [Evaluation](evaluation/README.md) - Runtime execution model
- [AST Specification](ast.md) - Abstract syntax tree structure
- [IR Specification](ir.md) - Intermediate representation
