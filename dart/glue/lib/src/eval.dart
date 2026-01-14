import 'dart:async';
import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/exception.dart';

/// ============================================================================
/// EVALUATION MONAD TYPE AND INSTANCES
/// ============================================================================

/// Evaluation monad for Glue expressions
/// Mirrors Haskell Glue.Eval.Eval exactly
class Eval<T> {
  final FutureOr<Either<EvalError, (T, Runtime)>> Function(Runtime) _run;

  const Eval(this._run);

  /// Create a successful evaluation
  static Eval<T> pure<T>(T value) => Eval((runtime) => Right((value, runtime)));

  /// Map over the result
  Eval<U> map<U>(U Function(T) f) => Eval((runtime) async {
    final result = await runEval(this, runtime);
    return result.match((error) => Left<EvalError, (U, Runtime)>(error), (
      value,
    ) {
      final (result, runtime) = value;
      return Right<EvalError, (U, Runtime)>((f(result), runtime));
    });
  });

  /// FlatMap (bind) operation
  Eval<U> flatMap<U>(Eval<U> Function(T) f) => Eval((runtime) async {
    final result = await runEval(this, runtime);
    return result.match((error) => Left<EvalError, (U, Runtime)>(error), (
      value,
    ) {
      final (result, runtime) = value;
      return runEval(f(result), runtime);
    });
  });

  /// Transform the evaluation result
  Eval<U> transform<U>(
    Either<EvalError, (U, Runtime)> Function(T, Runtime) f,
  ) => Eval((runtime) async {
    final result = await runEval(this, runtime);
    return result.match((error) => Left<EvalError, (U, Runtime)>(error), (
      value,
    ) {
      final (result, runtime) = value;
      return f(result, runtime);
    });
  });
}

/// ============================================================================
/// BASIC EVALUATION API
/// ============================================================================

/// Run the evaluation with initial runtime (matches Haskell runEval)
FutureOr<Either<EvalError, (T, Runtime)>> runEval<T>(
  Eval<T> eval,
  Runtime runtime,
) => eval._run(runtime);

/// Simple evaluation with just environment
/// Mirrors Haskell runEvalSimple exactly
FutureOr<Either<EvalError, (T, Runtime)>> runEvalSimple<T>(
  Eval<T> action,
  Env initialEnv,
) async {
  final initialRuntime = Runtime.initial(initialEnv);
  return runEval(action, initialRuntime);
}

/// Throw an evaluation error
Eval<T> throwError<T>(RuntimeException exception) =>
    Eval((runtime) => Left(EvalError(runtime.context, exception)));

/// Lift an IO operation into the Eval monad
Eval<T> liftIO<T>(FutureOr<T> io) => Eval((runtime) async {
  try {
    final result = await io;
    return Right((result, runtime));
  } catch (e) {
    // Convert exceptions to EvalError
    final exception = RuntimeException('io-error', IrString(e.toString()));
    final error = EvalError(runtime.context, exception);
    return Left(error);
  }
});

/// ============================================================================
/// ENVIRONMENT AND RUNTIME ACCESS
/// ============================================================================

/// Get current environment
Eval<Env> getEnv() => Eval((runtime) => Right((runtime.env, runtime)));

/// Set current environment
Eval<void> putEnv(Env env) =>
    Eval((runtime) => Right(((), runtime.copyWith(env: env))));

/// Get root environment
Eval<Env> getRootEnv() => Eval((runtime) => Right((runtime.rootEnv, runtime)));

/// Set root environment
Eval<void> putRootEnv(Env rootEnv) =>
    Eval((runtime) => Right(((), runtime.copyWith(rootEnv: rootEnv))));

/// Get current context
Eval<Context> getContext() =>
    Eval((runtime) => Right((runtime.context, runtime)));

/// Push context frame
Eval<void> pushContext(String name) => Eval(
  (runtime) =>
      Right(((), runtime.copyWith(context: [name, ...runtime.context]))),
);

/// Pop context frame
Eval<void> popContext() => Eval(
  (runtime) => runtime.context.isEmpty
      ? Left(
          EvalError(
            runtime.context,
            RuntimeException(
              'context-error',
              IrString('Cannot pop empty context'),
            ),
          ),
        )
      : Right(((), runtime.copyWith(context: runtime.context.sublist(1)))),
);

/// Get module registry
Eval<ModuleRegistry> getRegistry() =>
    Eval((runtime) => Right((runtime.registry, runtime)));

/// Get import cache
Eval<ImportedModuleCache> getCache() =>
    Eval((runtime) => Right((runtime.importCache, runtime)));

/// Set import cache
Eval<void> putCache(ImportedModuleCache cache) =>
    Eval((runtime) => Right(((), runtime.copyWith(importCache: cache))));

/// Get complete runtime
Eval<Runtime> getRuntime() => Eval((runtime) => Right((runtime, runtime)));

/// Set complete runtime
Eval<void> putRuntime(Runtime newRuntime) =>
    Eval((_) => Right(((), newRuntime)));

/// ============================================================================
/// VARIABLE MANAGEMENT
/// ============================================================================

/// Define a variable in current environment
Eval<void> defineVarEval(String name, Ir value) => Eval(
  (runtime) =>
      Right(((), runtime.copyWith(env: defineVar(name, value, runtime.env)))),
);

/// Update a variable in current environment
Eval<void> updateVarEval(String name, Ir value) => Eval((runtime) {
  final result = updateVar(name, value, runtime.env);
  return result.match(
    (error) => Left(EvalError(runtime.context, error)),
    (env) => Right(((), runtime.copyWith(env: env))),
  );
});

/// ============================================================================
/// ENVIRONMENT UTILITIES
/// ============================================================================

/// Run evaluation with temporary environment
Eval<T> withEnv<T>(Env tempEnv, Eval<T> action) => Eval((runtime) async {
  final originalEnv = runtime.env;
  final tempRuntime = runtime.copyWith(env: tempEnv);
  final result = await runEval(action, tempRuntime);
  return result.match((error) => Left<EvalError, (T, Runtime)>(error), (value) {
    final (result, runtime) = value;
    return Right<EvalError, (T, Runtime)>((
      result,
      runtime.copyWith(env: originalEnv),
    ));
  });
});

/// Run evaluation with additional context frame
Eval<T> withContext<T>(String contextName, Eval<T> action) => pushContext(
  contextName,
).flatMap((_) => action.flatMap((value) => popContext().map((_) => value)));

/// ============================================================================
/// EVALUATION SEQUENCING
/// ============================================================================

/// Sequence two evaluations
Eval<(T1, T2)> sequence<T1, T2>(Eval<T1> first, Eval<T2> second) =>
    first.flatMap((a) => second.map((b) => (a, b)));

/// Sequence multiple evaluations
Eval<List<T>> sequenceAll<T>(List<Eval<T>> evals) {
  if (evals.isEmpty) return Eval.pure([]);

  return evals[0].flatMap(
    (first) => sequenceAll(evals.sublist(1)).map((rest) => [first, ...rest]),
  );
}

/// Evaluate multiple expressions and return the last result
Eval<T> sequence_<T>(List<Eval<dynamic>> evals, Eval<T> last) {
  if (evals.isEmpty) return last;

  return evals[0].flatMap((_) => sequence_(evals.sublist(1), last));
}

/// ============================================================================
/// CORE EXPRESSION EVALUATION
/// ============================================================================

/// Main evaluation function - evaluates IR expressions
/// Mirrors Haskell Glue.Eval.eval exactly
Eval<Ir> eval(Ir ir) {
  return switch (ir) {
    IrSymbol(:final value) => evalSymbol(value),
    IrDottedSymbol(:final parts) => evalDottedSymbol(parts),
    IrList(:final elements) => evalList(elements.unlock),
    IrObject(:final properties) => evalObject(properties.unlock),
    // Literals evaluate to themselves
    _ => Eval.pure(ir),
  };
}

/// Evaluate a symbol by looking it up in the environment
Eval<Ir> evalSymbol(String name) {
  return getEnv().flatMap((env) {
    final result = lookupVar(name, env);
    return result.match(
      (error) => throwError(error),
      (value) => Eval.pure(value),
    );
  });
}

/// Evaluate dotted symbol access (module.property.field)
Eval<Ir> evalDottedSymbol(List<String> parts) {
  if (parts.isEmpty) {
    return throwError(
      RuntimeException('invalid-symbol', IrString('Empty dotted symbol')),
    );
  }

  if (parts.length == 1) {
    return evalSymbol(parts[0]);
  }

  // Find the longest prefix that exists as a symbol
  return _evalWithPrefixes(parts);
}

/// Evaluate a list (function call or literal list)
/// Mirrors Haskell evalList exactly
Eval<Ir> evalList(List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure(IrList([]));
  }

  final first = elements[0];

  // Check if it's a single symbol (variable lookup)
  if (elements.length == 1 && first is IrSymbol) {
    return withContext(
      first.value,
      getEnv().flatMap((env) {
        final result = lookupVar(first.value, env);
        return result.match(
          (error) => throwError(error),
          (value) => switch (_isCallable(value)) {
            true => apply(value, []),
            false => Eval.pure(value),
          },
        );
      }),
    );
  }

  // Check if it starts with a symbol (function call)
  if (first is IrSymbol) {
    final name = first.value;
    final args = elements.sublist(1);
    return withContext(
      name,
      getEnv().flatMap((env) {
        final result = lookupVar(name, env);
        return result.match(
          (error) => throwError(error),
          (value) => apply(value, args),
        );
      }),
    );
  }

  // Otherwise, evaluate all elements and create a list
  return withContext(
    '<call>',
    sequenceAll(elements.map(eval).toList()).flatMap((evaluated) {
      if (evaluated.isNotEmpty && _isCallable(evaluated[0])) {
        return apply(evaluated[0], evaluated.sublist(1));
      }
      return Eval.pure(IrList(evaluated));
    }),
  );
}

/// Evaluate an object
Eval<Ir> evalObject(Map<String, Ir> properties) {
  return sequenceAll(properties.values.map(eval).toList()).map((
    evaluatedValues,
  ) {
    final evaluatedProps = <String, Ir>{};
    var i = 0;
    for (final key in properties.keys) {
      evaluatedProps[key] = evaluatedValues[i++];
    }
    return IrObject(evaluatedProps);
  });
}

/// ============================================================================
/// FUNCTION APPLICATION
/// ============================================================================

/// Apply a function to arguments
Eval<Ir> apply(Ir func, List<Ir> args) {
  return switch (func) {
    IrNative(value: final f) => applyNative(f, args),
    IrClosure(params: final params, body: final body, env: final closureEnv) =>
      applyClosure(params, body, closureEnv, args),
    IrSymbol(value: final name) => throwError(unboundVariable(name)),
    _ => throwError(notCallableObject()),
  };
}

/// Apply a native function/special form
Eval<Ir> applyNative(Native native, List<Ir> args) {
  return switch (native) {
    NativeFunc(function: final f) => _applyNativeFunc(f, args),
    NativeSpecial(function: final s) => s(
      args,
    ), // Special forms handle their own evaluation
  };
}

/// Apply a native function (evaluate arguments first)
Eval<Ir> _applyNativeFunc(dynamic func, List<Ir> rawArgs) {
  return sequenceAll(rawArgs.map(eval).toList()).flatMap((args) => func(args));
}

/// Apply a closure with the given arguments
Eval<Ir> applyClosure(
  List<String> params,
  Ir body,
  Env closureEnv,
  List<Ir> rawArgs,
) {
  final numArgs = rawArgs.length;
  final numParams = params.length;

  if (numArgs == numParams) {
    // Full application: execute the function
    return _applyFullClosure(params, body, closureEnv, rawArgs);
  } else if (numArgs < numParams) {
    // Partial application: create new closure
    return _applyPartialClosure(params, body, closureEnv, rawArgs);
  } else {
    // Too many arguments
    return throwError(wrongNumberOfArguments());
  }
}

/// ============================================================================
/// HELPER FUNCTIONS
/// ============================================================================

/// Helper to find the longest prefix that exists
Eval<Ir> _evalWithPrefixes(List<String> parts) {
  return getEnv().flatMap((env) {
    // Try prefixes from longest to shortest
    for (final prefix in _generatePrefixes(parts)) {
      final prefixName = prefix.join('.');
      final result = lookupVar(prefixName, env);

      if (result.isRight) {
        // Found the prefix, navigate the remaining parts
        return result.match(
          (_) => throwError(unboundVariable(parts.join('.'))),
          (value) => _evalNestedAccess(value, parts.sublist(prefix.length)),
        );
      }
      // Continue to next prefix if not found
    }

    // No prefix found
    return throwError(unboundVariable(parts.join('.')));
  });
}

/// Generate all proper prefixes of a symbol path
List<List<String>> _generatePrefixes(List<String> parts) {
  final prefixes = <List<String>>[];
  for (var i = parts.length; i > 0; i--) {
    prefixes.add(parts.sublist(0, i));
  }
  return prefixes;
}

/// Navigate nested object/module access
Eval<Ir> _evalNestedAccess(Ir obj, List<String> remainingParts) {
  if (remainingParts.isEmpty) {
    return Eval.pure(obj);
  }

  final prop = remainingParts[0];
  final rest = remainingParts.sublist(1);

  return switch (obj) {
    IrObject(properties: final props) =>
      props[prop] != null
          ? _evalNestedAccess(props[prop]!, rest)
          : throwError(propertyNotFound(prop)),
    IrModule() =>
      // Module access - for now, treat as not found
      // This will be implemented when we add module import
      throwError(
        RuntimeException(
          'module-access',
          IrString('Module access not yet implemented'),
        ),
      ),
    _ => throwError(notAnObject(obj)),
  };
}

/// Check if an IR value can be called
bool _isCallable(Ir value) {
  return switch (value) {
    IrNative() => true,
    IrClosure() => true,
    _ => false,
  };
}

/// Full application of a closure
Eval<Ir> _applyFullClosure(
  List<String> params,
  Ir body,
  Env closureEnv,
  List<Ir> rawArgs,
) {
  return sequenceAll(rawArgs.map(eval).toList()).flatMap((args) {
    final bindings = <(String, Ir)>[];
    for (var i = 0; i < params.length; i++) {
      bindings.add((params[i], args[i]));
    }
    return withEnv(_buildEnvWithBindings(closureEnv, bindings), eval(body));
  });
}

/// Partial application of a closure
Eval<Ir> _applyPartialClosure(
  List<String> params,
  Ir body,
  Env closureEnv,
  List<Ir> rawArgs,
) {
  return sequenceAll(rawArgs.map(eval).toList()).map((args) {
    final (usedParams, remainingParams) = _splitParams(params, args.length);
    final bindings = <(String, Ir)>[];
    for (var i = 0; i < usedParams.length; i++) {
      bindings.add((usedParams[i], args[i]));
    }
    final partiallyAppliedEnv = _buildEnvWithBindings(closureEnv, bindings);
    return IrClosure(remainingParams, body, partiallyAppliedEnv);
  });
}

/// Split parameters for partial application
(List<String>, List<String>) _splitParams(List<String> params, int numUsed) {
  final used = params.sublist(0, numUsed);
  final remaining = params.sublist(numUsed);
  return (used, remaining);
}

/// Build environment with parameter bindings
Env _buildEnvWithBindings(Env env, List<(String, Ir)> bindings) {
  var currentEnv = env;
  for (final (param, value) in bindings) {
    currentEnv = defineVar(param, value, currentEnv);
  }
  return currentEnv;
}

/// Evaluate function body with implicit sequence semantics
/// Mirrors Haskell evalBody exactly
Eval<Ir> _evalBody(Ir body) {
  return eval(body).flatMap((result) {
    return switch (result) {
      IrList(elements: final xs) => xs.isEmpty
          ? Eval.pure(IrVoid())
          : Eval.pure(xs.last),
      _ => Eval.pure(result),
    };
  });
}
