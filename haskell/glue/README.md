# 🚀 Glue Haskell Implementation

> Reference implementation of the Glue programming language in Haskell — a complete interpreter with parser, evaluator, and standard library.

[![Haskell](https://img.shields.io/badge/Language-Haskell-5e5086.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-BSD--3--Clause-blue.svg)](../../../LICENSE)

## 📋 Overview

This is the reference implementation of the Glue programming language, written in Haskell. It provides a complete interpreter that can parse, compile, and evaluate Glue code, serving as both a working implementation and a reference for other implementations.

### 🎯 Key Features

- **Complete Parser**: Megaparsec-based parser for full Glue syntax
- **AST/IR Compilation**: Transforms Abstract Syntax Tree to Intermediate Representation
- **Runtime Evaluator**: Safe evaluation with lexical scoping and error handling
- **Standard Library**: Comprehensive built-in functions for data manipulation
- **Example Runner**: Command-line tool demonstrating language features
- **Extensive Testing**: Full test suite with HSpec and QuickCheck

## 🏗️ Architecture

### Core Components

#### Parser (`Glue.Parser`)
- **Megaparsec-based**: Robust parsing with detailed error messages
- **Full Syntax Support**: Handles all Glue constructs (atoms, lists, property objects, etc.)
- **Error Recovery**: Graceful handling of syntax errors

#### AST/IR System (`Glue.AST`, `Glue.IR`)
- **AST**: Direct representation of parsed syntax
- **IR**: Optimized intermediate representation for execution
- **Compilation**: AST to IR transformation with semantic analysis

#### Evaluator (`Glue.Eval`)
- **Lexical Scoping**: Proper variable binding and closure capture
- **Type Safety**: Runtime type checking and validation
- **Error Handling**: Comprehensive error reporting and context

#### Standard Library (`Glue.Lib.*`)
- **Bool Operations**: Conditionals, comparisons, logical operations
- **List Functions**: Manipulation, filtering, transformation operations
- **Math Operations**: Arithmetic, trigonometric, logarithmic functions
- **Built-in Special Forms**: def, set, lambda, etc.

### Execution Pipeline

```
Source Code → Parser → AST → Compiler → IR → Evaluator → Result
```

## 🚀 Getting Started

### Prerequisites

- **GHC**: Glasgow Haskell Compiler (9.8+ recommended)
- **Cabal**: Haskell package manager

### Building

```bash

# Install dependencies and build
cabal build glue

# Run tests
cabal test glue

# Install executable
cabal install glue
```

### Usage

#### Example Runner

```bash
# Run the example runner
cabal run glue

# Or after installation
glue
```

The tool evaluates a series of example expressions demonstrating language features, showing parsing and evaluation results.

#### Programmatic Usage

```haskell
import Glue.Parser (parseGlue)
import Glue.IR (compile)
import Glue.Eval (eval, runEval)

-- Parse and evaluate Glue code
main :: IO ()
main = do
    let code = "(+ 1 2 3)"
    case parseGlue code of
        Left err -> print err
        Right ast -> do
            let initialEnv = E.fromFrame lib
            result <- runEval (eval (compile ast)) initialEnv
            case result of
                Left err -> print err
                Right (value, _) -> print value
```

## 📁 Project Structure

```
haskell/glue/
├── glue.cabal          # Package configuration
├── app/Main.hs         # REPL executable
├── src/
│   ├── Glue.hs         # Main module
│   ├── Glue/
│   │   ├── AST.hs      # Abstract Syntax Tree
│   │   ├── Parser.hs   # Parser implementation
│   │   ├── IR.hs       # Intermediate Representation
│   │   ├── Eval.hs     # Evaluator
│   │   ├── Env.hs      # Environment management
│   │   ├── Error.hs    # Error types
│   │   ├── Lib/        # Standard library
│   │   │   ├── Bool.hs     # Boolean operations
│   │   │   ├── Builtin.hs  # Built-in functions
│   │   │   ├── List.hs     # List operations
│   │   │   └── Math.hs     # Math operations
│   │   └── Parser/
│   │       └── Error.hs    # Parser errors
│   └── Spec/
│       └── Device.hs   # Device specifications
└── test/
    ├── Spec.hs         # Test runner
    └── Glue/           # Test modules
        ├── EvalSpec.hs
        ├── ParserSpec.hs
        └── Lib/*/      # Library tests
```

## 🧪 Testing

The implementation includes comprehensive tests:

```bash
# Run all tests
cabal test glue

# Run specific test suites
cabal test glue --test-options="--match=Parser"
cabal test glue --test-options="--match=Eval"
```

### Test Coverage

- **Parser Tests**: Syntax validation and AST construction
- **Evaluator Tests**: Runtime behavior and edge cases
- **Library Tests**: Standard function correctness
- **Property Tests**: QuickCheck-based property validation

## 🔌 Integration

### Embedding in Applications

The implementation is designed for easy embedding:

```haskell
import Glue.Eval (Runtime(..), runEval)

-- Create evaluation environment
let env = E.fromFrame lib
let state = Runtime { env = env, context = [] }

-- Evaluate Glue code
result <- runEval (eval compiledIR) state
```

### FFI Support

The evaluator supports host function integration through the environment system, enabling seamless FFI with Haskell functions.

## 📊 Performance

- **Parsing**: Efficient recursive descent with Megaparsec
- **Evaluation**: Tree-walking interpreter with tail call optimization
- **Memory**: Minimal footprint suitable for embedding
- **Safety**: Comprehensive error handling prevents crashes

## 🤝 Contributing

Contributions to the implementation should:

1. Follow the [language specification](../../../spec/)
2. Include comprehensive tests
3. Maintain type safety and error handling
4. Update documentation as needed

## 📄 License

BSD 3-Clause License - see LICENSE file for details.

## 🔗 Related

- **[Language Specification](../../../spec/)**: Complete Glue language specification
- **[Examples](../../../examples/)**: Sample Glue code
- **[VS Code Extension](../../../glue-ext/)**: Language support tools

---

*Reference implementation of Glue: Safe, embeddable, and thoroughly tested.*
