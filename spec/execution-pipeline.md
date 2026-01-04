
# 🔄 Glue Execution Pipeline

Glue transforms source code through this carefully designed pipeline:

```
Source Text → Parse → AST → Compile → IR → Prepare Environment → Register Modules → Create EvalState → Evaluate → Result
```

## Pipeline Stages

### 1. Source Text
Human-readable Glue code input as text.

### 2. Parse
Convert text to Abstract Syntax Tree (AST) using the language grammar. Validates syntax and creates structured representation.  
*See: [Parsing to AST](parsing-to-ast.md)*

### 3. AST (Abstract Syntax Tree)
Tree structure representing the syntactic structure of the program. Contains nodes for atoms, lists, property objects.  
*See: [AST Specification](ast.md)*

### 4. Compile
Transform AST into Intermediate Representation (IR). Performs semantic analysis, type checking, and optimization.  
*See: [Compilation: AST to IR](compilation-ast-ir.md)*

### 5. IR (Intermediate Representation)
Lower-level representation optimized for execution. Contains primitives, closures, objects, native function calls and modules.  
*See: [IR Specification](ir.md)*

### 6. Prepare Environment
Set up the execution environment with built-in and custom functions, constants.  
*See: [Environment](environment.md)*

### 7. Register Modules
Register external modules, making their exports available in the environment.  
*See: [Module System](module-system.md)*

### 8. Create EvalState
Complete execution context with environment, modules registry and cache, evaluation context.  
*See: [Evaluation State](evaluation/evaluation-state.md)*

### 9. Evaluate
Execute the IR using the evaluation semantics, applying functions, evaluating conditionals, and performing computations.  
*See: [Evaluation](evaluation/README.md)*

### 10. Result
Final computed value or side effects produced by the program execution.

## Key Benefits

- **Safety**: Each stage validates and transforms safely
- **Performance**: Optimized IR enables efficient execution
- **Modularity**: Clear separation of concerns
- **Debuggability**: Pipeline stages aid error diagnosis

## Related Documents

- [Language Overview](language-overview.md) - High-level introduction
- [AST Specification](ast.md) - Abstract syntax tree structure
- [IR Specification](ir.md) - Intermediate representation
- [Evaluation](evaluation/README.md) - Runtime execution model
