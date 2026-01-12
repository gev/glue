# Glue Language Interpreter

A complete implementation of the Glue programming language in Dart, featuring Lisp-inspired syntax with modern enhancements for functional programming, property-based objects, and module systems.

## Features

- **Lisp-inspired syntax** with parentheses for calls and `(:key value)` for objects
- **Functional programming** with closures, partial application, and higher-order functions
- **Property-based objects** for structured data with dot notation access
- **Module system** with lazy loading, caching, and namespace isolation
- **Extensible evaluation** through native functions and special forms
- **Async evaluation** with proper error handling and stack traces

## Installation

```bash
dart pub add glue
```

## Usage

```dart
import 'package:glue/glue.dart';

void main() async {
  final glue = GlueInterpreter();

  // Simple arithmetic
  final result = await glue.evalString('(+ 1 2 3)');
  print(result); // Output: 6

  // Functions and recursion
  final factorial = await glue.evalString('''
    (def factorial (lambda (n)
      (if (= n 0)
          1
          (* n (factorial (- n 1))))))
    factorial
  ''');

  final fact5 = await glue.evalString('(factorial 5)');
  print(fact5); // Output: 120

  // Objects and properties
  final user = await glue.evalString('''
    (:name "Alice" :age 30 :profile (:theme "dark" :lang "dart"))
  ''');
  print(user); // Object with properties
}
```

## Architecture

The interpreter follows a clean, layered architecture:

1. **Parser** (`petitparser`) → AST (abstract syntax tree)
2. **Compiler** → IR (intermediate representation)
3. **Evaluator** → Runtime execution with lexical environment
4. **Modules** → Lazy loading and caching system

### Key Components

- **AST/IR**: Sealed classes for exhaustive pattern matching
- **Parser**: Petitparser combinators for Lisp syntax
- **Environment**: Frame-based scoping with proper closures
- **Evaluation**: Async evaluation with error propagation
- **Modules**: Isolated evaluation with import/export semantics

## Development

This package implements the Glue language specification. See `/dart/todo.md` for the complete implementation roadmap and technical decisions.

### Running Tests

```bash
dart test
```

### Building

```bash
dart pub get
dart compile exe bin/glue.dart  # If you add a CLI
```

## Examples

See the `/example` directory for more comprehensive usage examples including:

- Basic arithmetic and functions
- Object manipulation
- Module usage
- Error handling
- Custom native functions

## Contributing

Contributions welcome! The implementation follows strict guidelines:

- **Type Safety**: Sealed classes for AST/IR types
- **Testing**: Comprehensive unit and integration tests
- **Performance**: Optimized evaluation and memory usage
- **Architecture**: Clean separation of concerns

### Development Setup

1. Clone the repository
2. Run `dart pub get` in the `dart/glue` directory
3. Run tests with `dart test`
4. Check code with `dart analyze`

## License

Part of the Glue project - see main LICENSE file.
