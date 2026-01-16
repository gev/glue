# Haskell Reference Directory Structure

This document provides the complete directory structure for the Haskell reference implementation of the Glue programming language.

## Complete Haskell Directory Structure

```
haskell/glue/
├── glue.cabal              # Project configuration
├── README.md               # Documentation
├── app/Main.hs             # Executable entry point
├── src/Glue/               # Core language (12 files)
│   ├── AST.hs              # Abstract Syntax Tree
│   ├── Env.hs              # Environment management
│   ├── Error.hs            # Error handling types
│   ├── Eval.hs             # Expression evaluation
│   ├── IR.hs               # Intermediate Representation
│   ├── Module.hs           # Module system core
│   ├── Parser.hs           # Source code parsing
│   ├── Eval/Error.hs       # Evaluation errors
│   ├── Eval/Exception.hs   # Runtime exceptions
│   ├── Module/Cache.hs     # Module caching
│   ├── Module/Error.hs     # Module errors
│   ├── Module/Loader.hs    # Module loading
│   ├── Module/Registration.hs # Module registration
│   ├── Module/Registry.hs  # Module registry
│   └── Parser/Error.hs     # Parser errors
└── src/Glue/Lib/           # Standard libraries (66 files)
    ├── Bool.hs             # Bool library main
    ├── Bool/               # 13 files (12 functions + main)
    │   ├── Eq.hs           # Equality operations
    │   ├── Ge.hs           # Greater or equal
    │   ├── Gt.hs           # Greater than
    │   ├── If.hs           # Conditional execution
    │   ├── Le.hs           # Less or equal
    │   ├── Lt.hs           # Less than
    │   ├── Ne.hs           # Not equal
    │   ├── Not.hs          # Logical not
    │   ├── Until.hs        # Loop until
    │   ├── When.hs         # Conditional when
    │   └── While.hs        # Loop while
    ├── Builtin.hs          # Builtin functions main
    ├── Builtin/            # 8 files (7 functions + main)
    │   ├── Def.hs          # Variable definition
    │   ├── Error.hs        # Error handling
    │   ├── Import.hs       # Module imports
    │   ├── Lambda.hs       # Lambda functions
    │   ├── Let.hs          # Local bindings
    │   ├── Set.hs          # Variable assignment
    │   └── Try.hs          # Exception handling
    ├── IO.hs               # IO library main
    ├── IO/                 # 3 files (2 functions + main)
    │   ├── Print.hs        # Output functions
    │   └── Read.hs         # Input functions
    ├── List.hs             # List library main
    ├── List/               # 22 files (21 functions + main)
    │   ├── Append.hs       # List concatenation
    │   ├── Butlast.hs      # All but last element
    │   ├── Car.hs          # First element
    │   ├── Cdr.hs          # Rest of list
    │   ├── Cons.hs         # Construct list
    │   ├── Drop.hs         # Drop elements
    │   ├── Filter.hs       # Filter elements
    │   ├── Find.hs         # Find element
    │   ├── Flatten.hs      # Flatten nested lists
    │   ├── Last.hs         # Last element
    │   ├── Length.hs       # List length
    │   ├── Map.hs          # Map function
    │   ├── Member.hs       # Membership test
    │   ├── Nth.hs          # Nth element
    │   ├── Partition.hs    # Partition list
    │   ├── Position.hs     # Element position
    │   ├── Remove.hs       # Remove elements
    │   ├── Reverse.hs      # Reverse list
    │   ├── Sort.hs         # Sort list
    │   ├── Take.hs         # Take elements
    │   └── Zip.hs          # Zip lists
    └── Math/               # 23 files (arithmetic complete)
        ├── Arithmetic.hs   # Arithmetic main
        ├── Const.hs        # Mathematical constants
        ├── Logarithmic.hs  # Logarithmic functions main
        ├── Power.hs        # Power functions main
        ├── Trigonometric.hs# Trigonometric functions main
        ├── Utility.hs      # Math utilities main
        └── Arithmetic/     # 5 submodules
        │   ├── Add.hs      # Addition
        │   ├── Div.hs      # Division
        │   ├── Mod.hs      # Modulo
        │   ├── Mul.hs      # Multiplication
        │   └── Sub.hs      # Subtraction
        └── Logarithmic/    # 3 submodules
        │   ├── Lg.hs       # Base-10 logarithm
        │   ├── Ln.hs       # Natural logarithm
        │   └── Log.hs      # Arbitrary base logarithm
        └── Power/          # 3 submodules
        │   ├── Exp.hs      # Exponential function
        │   ├── Pow.hs      # Power function
        │   └── Sqrt.hs     # Square root
        └── Trigonometric/  # 6 submodules
        │   ├── Acos.hs     # Arc cosine
        │   ├── Asin.hs     # Arc sine
        │   ├── Atan.hs     # Arc tangent
        │   ├── Cos.hs      # Cosine
        │   ├── Sin.hs      # Sine
        │   └── Tan.hs      # Tangent
        └── Utility/        # 7 submodules
            ├── Abs.hs      # Absolute value
            ├── Ceil.hs     # Ceiling function
            ├── Floor.hs    # Floor function
            ├── Max.hs      # Maximum value
            ├── Min.hs      # Minimum value
            ├── Round.hs    # Round to nearest
            └── Trunc.hs    # Truncate decimal

test/Glue/                  # Test suite (22 files)
├── Spec.hs                 # Test runner
├── TestUtils.hs            # Test utilities
├── CompileSpec.hs          # Compilation tests
├── EnvSpec.hs              # Environment tests
├── EvalSpec.hs             # Evaluation tests
├── ParserSpec.hs           # Parser tests
├── Lib/Bool/               # 12 test files
├── Lib/Builtin/            # 6 test files (Def, Error, Import, Lambda, Set, Try)
├── Lib/IO/                 # 1 test file
├── Lib/List/               # 21 test files
├── Lib/Math/               # Partial test files
└── Module/                 # 3 test files
```

## File Count Summary

- **Total Files**: 178
- **Source Files**: 135 (.hs files)
- **Test Files**: 22 (Spec.hs files)
- **Config Files**: 3 (glue.cabal, README.md, etc.)
- **Core Language**: 12 files
- **Standard Libraries**: 66 files
- **Test Suite**: 22 files

## Directory Breakdown

| Directory | Files | Description |
|-----------|-------|-------------|
| `src/Glue/` | 12 | Core language implementation |
| `src/Glue/Lib/Bool/` | 13 | Boolean operations library |
| `src/Glue/Lib/Builtin/` | 8 | Builtin functions library |
| `src/Glue/Lib/IO/` | 3 | Input/Output library |
| `src/Glue/Lib/List/` | 22 | List operations library |
| `src/Glue/Lib/Math/` | 23 | Mathematical functions library |
| `test/Glue/` | 22 | Complete test suite |

This structure serves as the reference implementation for the Glue programming language, providing a complete functional programming environment with comprehensive standard libraries.
