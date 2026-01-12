# Glue Language Architecture in Dart

## Overview

The Glue language interpreter for Dart implements a Lisp-inspired programming language with modern features for functional programming, property-based objects, and module systems. The architecture follows a clean, layered design optimized for Dart's async/await patterns and Flutter integration.

## Core Architecture

### System Layers

```
┌─────────────────────────────────────┐
│           User Code                 │
│         (Glue Scripts)              │
├─────────────────────────────────────┤
│         Interpreter API             │
│    (GlueInterpreter.evalString())   │
├─────────────────────────────────────┤
│         Parser Layer                │
│    (petitparser → AST)              │
├─────────────────────────────────────┤
│         Compiler Layer              │
│       (AST → IR)                    │
├─────────────────────────────────────┤
│         Evaluation Layer            │
│    (IR → Runtime Execution)         │
├─────────────────────────────────────┤
│         Runtime System              │
│  (Environment, Modules, IO)         │
├─────────────────────────────────────┤
│         Host Integration            │
│    (Dart/Flutter Bridge)            │
└─────────────────────────────────────┘
```

## Component Architecture

### 1. Parser Layer (`petitparser`)

**Purpose**: Convert Glue source text to Abstract Syntax Tree (AST)

**Components**:
- `GlueParser`: Main parser class using petitparser combinators
- `Ast` hierarchy: Sealed classes for syntax nodes
- Error recovery and detailed error reporting

**Key Design**: Parser combinators provide composable, testable parsing logic with excellent error messages.

### 2. Compiler Layer (AST → IR)

**Purpose**: Transform AST to Intermediate Representation optimized for execution

**Components**:
- `Ir` hierarchy: Sealed classes for runtime values
- Compilation functions: `compile(ast)` transforms syntax to executable form
- Type checking and optimization passes

**Key Design**: IR focuses on execution semantics, enabling efficient evaluation and optimization.

### 3. Evaluation Engine (Core Runtime)

**Purpose**: Execute IR in the runtime environment

#### Eval Monad Implementation

The heart of the system is the `Eval` monad, implemented as an async class:

```dart
class Eval<A> {
  final Future<EvalResult<A>> Function(Runtime) runEval;

  Future<EvalResult<A>> execute(Runtime runtime) => runEval(runtime);

  Eval<B> flatMap<B>(Eval<B> Function(A) f) {
    return Eval((runtime) async {
      final result = await runEval(runtime);
      return result.fold(
        (error) => EvalResult.error(error, runtime),
        (success) => f(success.value).runEval(success.runtime),
      );
    });
  }
}
```

**Why Future-based Monad?**
- **IO Handling**: Dart's primary mechanism for side effects
- **Flutter Compatibility**: Natural integration with FutureBuilder
- **Async Composition**: Clean chaining of async operations
- **Error Propagation**: Structured error handling with Either types

### 4. Runtime System

#### Environment Management

**Frame-based Scoping**:
```dart
class Environment {
  final List<Map<String, Ir>> frames; // Stack of scopes

  Ir? lookup(String name) {
    for (final frame in frames.reversed) {
      final value = frame[name];
      if (value != null) return value;
    }
    return null;
  }
}
```

**Key Features**:
- Lexical scoping with proper closure capture
- Efficient frame pushing/popping
- Shadowing support

#### Module System

**Lazy Loading Architecture**:
```dart
class ModuleRegistry {
  final Map<String, ModuleInfo> _modules = {};
  final Map<String, ImportedModule> _cache = {};

  Future<ImportedModule> import(String name) async {
    if (_cache.containsKey(name)) {
      return _cache[name]!;
    }
    // Load and evaluate module
    final module = await _loadModule(name);
    _cache[name] = module;
    return module;
  }
}
```

**Features**:
- Isolated evaluation environments
- Caching for performance
- Hierarchical namespace resolution

### 5. Host Integration Layer

#### Native Functions Bridge

```dart
class NativeFunctionRegistry {
  final Map<String, NativeIr> _functions = {};

  void register(String name, Future<Ir> Function(List<Ir>) implementation) {
    _functions[name] = NativeIr.func(implementation);
  }
}
```

#### Flutter Integration (Future Possibility)

The async nature enables powerful Flutter integration patterns:

```dart
class GlueWidget extends StatelessWidget {
  final String glueCode;

  @override
  Widget build(BuildContext context) {
    return FutureBuilder<EvalResult<Widget>>(
      future: GlueInterpreter().evalViewCode(glueCode),
      builder: (context, snapshot) {
        if (!snapshot.hasData) return CircularProgressIndicator();

        return snapshot.data!.fold(
          (error) => ErrorWidget(error.message),
          (success) => success.value,
        );
      },
    );
  }
}
```

## Data Flow Architecture

### Evaluation Pipeline

```
Source Code → Parser → AST → Compiler → IR → Evaluator → Result
     ↓         ↓        ↓        ↓        ↓        ↓
  String   petitparser Sealed  compile()  Sealed   Future<Ir>
                      Classes           Classes
```

### Error Handling Flow

```
Evaluation Error → Either<Error, Result> → Exception → User
Module Error     → Registry Exception     → User
Parse Error      → Parser Exception       → User
```

## Key Design Decisions

### 1. Sealed Classes for Type Safety

**AST/IR Hierarchies**:
```dart
sealed class Ast {}
class StringAst extends Ast { final String value; }
class IntegerAst extends Ast { final int value; }

sealed class Ir {}
class StringIr extends Ir { final String value; }
class IntegerIr extends Ir { final int value; }
```

**Benefits**:
- Exhaustive pattern matching
- Compiler-verified completeness
- Type-safe transformations

### 2. Async-First Evaluation

**All evaluation is async**:
- Natural for IO operations
- Flutter-compatible
- Enables reactive programming
- Supports cancellable computations

### 3. Functional Composition

**Monad-based architecture**:
- Clean error handling with Either
- Composable async operations
- Testable components
- Mathematical foundation

### 4. Environment Isolation

**Module-level isolation**:
- Each module evaluates in clean environment
- Controlled imports/exports
- Security boundaries
- Performance isolation

## Advanced Integration Patterns

### MVVM Architecture in Glue

**Key Architectural Insight**: Model, View, and ViewModel are not separate interpreters but different environments within the same Glue interpreter instance. The interpreter dynamically switches contexts to provide layer-specific functionality.

#### Single Interpreter, Multiple Environments

```
┌─────────────────────────────────────────────────────────────┐
│                    Glue Interpreter                         │
├─────────────────────────────────────────────────────────────┤
│  Runtime Context Switcher                                   │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ Model Environment:   Data/API functions             │    │
│  │ - Database queries, API calls, business logic       │    │
│  └─────────────────────────────────────────────────────┘    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ ViewModel Environment: Transformation logic         │    │
│  │ - Data formatting, validation, state management     │    │
│  └─────────────────────────────────────────────────────┘    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ View Environment:     UI widget constructors        │    │
│  │ - Flutter widget DSL, layout composition            │    │
│  └─────────────────────────────────────────────────────┘    │
├─────────────────────────────────────────────────────────────┤
│  Shared Runtime: Environment Stack, Module Registry         │
└─────────────────────────────────────────────────────────────┘
```

#### Environment Switching Mechanism

```dart
class GlueInterpreter {
  // Single interpreter instance
  final Runtime runtime;

  // Context switching methods
  Future<Ir> evalInModel(String code) async {
    return await _evalWithEnvironment(code, modelEnvironment);
  }

  Future<Ir> evalInViewModel(String code) async {
    return await _evalWithEnvironment(code, viewModelEnvironment);
  }

  Future<Widget> evalInView(String code) async {
    return await _evalWithEnvironment(code, viewEnvironment);
  }

  Future<Ir> _evalWithEnvironment(String code, Environment env) async {
    final previousEnv = runtime.environment;
    runtime.environment = env;
    try {
      return await evalString(code);
    } finally {
      runtime.environment = previousEnv; // Restore context
    }
  }
}
```

#### Runtime Module System

Each environment loads different runtime modules:

```dart
// Model Environment Modules
final modelModules = {
  'database': DatabaseModule(),
  'api': ApiModule(),
  'validation': BusinessLogicModule(),
};

// ViewModel Environment Modules
final viewModelModules = {
  'transform': DataTransformModule(),
  'format': UiFormatModule(),
  'state': StateManagementModule(),
};

// View Environment Modules
final viewModules = {
  'widgets': FlutterWidgetModule(),
  'layout': LayoutDslModule(),
  'animation': AnimationModule(),
};
```

### Server-Driven UI

```dart
// Glue code defining UI
final uiCode = '''
(form :title "Login"
  (column
    (text-field :label "Username" :validator validateUsername)
    (text-field :label "Password" :validator validatePassword)
    (button :label "Submit" :action submitLogin)))
''';

// Evaluates to Flutter widgets
final Widget loginForm = await interpreter.evalViewCode(uiCode);
```

## Performance Characteristics

### Memory Management
- **Structural sharing** in immutable collections
- **Lazy evaluation** of modules
- **Efficient frame management** with stack allocation

### Execution Performance
- **Direct interpretation** with precompiled cache
- **Optimized IR** for common patterns
- **Async batching** for IO operations

### Scalability
- **Modular architecture** enables parallel development
- **Caching layers** reduce redundant computation
- **Streaming evaluation** for large datasets

## Testing Architecture

### Component Isolation
- **Pure functions** tested independently
- **Mock environments** for controlled testing
- **Async testing** with proper timeouts

### Integration Testing
- **End-to-end evaluation** testing
- **Error propagation** validation

## Future Extensions

### Potential Enhancements
- **Persistent caching** of compiled modules
- **Concurrent evaluation** with isolates

### Flutter-Specific Features
- **State management** integration
- **Animation DSL** extensions

## Conclusion

The Glue interpreter architecture combines functional programming principles with Dart's practical async/await patterns, creating a powerful yet maintainable system. The monadic Eval structure provides clean composition while remaining idiomatic to Dart development.

The design enables both standalone language usage and deep integration with Flutter applications, supporting everything from simple scripting to complex reactive UI architectures.
