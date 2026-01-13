/// Glue Language Interpreter for Dart
///
/// A complete implementation of the Glue programming language featuring:
/// - Lisp-inspired syntax with modern enhancements
/// - Functional programming with closures and partial application
/// - Property-based objects with dot notation access
/// - Module system with lazy loading and caching
/// - Extensible evaluation through native functions
library;

export 'src/ast.dart';
export 'src/either.dart' hide Either;
export 'src/env.dart' hide Frame;
export 'src/eval.dart'
    show
        runEvalSimple,
        Eval,
        EvalIR,
        eval,
        evalSymbol,
        evalDottedSymbol,
        evalList,
        evalObject,
        apply,
        applyNative,
        applyClosure,
        getEnv,
        putEnv,
        getRootEnv,
        putRootEnv,
        getContext,
        pushContext,
        popContext,
        getRegistry,
        getCache,
        putCache,
        getRuntime,
        putRuntime,
        throwError,
        defineVarEval,
        updateVarEval,
        withEnv,
        withContext,
        sequence,
        sequenceAll,
        sequence_;
export 'src/lib/builtin.dart' show builtin;
export 'src/lib/builtin/lambda.dart' show lambda, extractSymbols;
export 'src/eval_error.dart';
export 'src/ir.dart' hide Env;
export 'src/module.dart';
export 'src/module_cache.dart';
export 'src/module_registration.dart';
export 'src/module_registry.dart';
export 'src/parser.dart';
export 'src/parser_errors.dart';
export 'src/runtime.dart';
export 'src/runtime_exceptions.dart';

// Standard Library Modules
export 'bool.dart';
export 'io.dart';
export 'list.dart';
export 'math.dart';

// TODO: Export additional modules as they are implemented
// export 'src/eval.dart';
// export 'src/module.dart';
