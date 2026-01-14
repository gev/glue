import 'dart:async';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart' hide Env;
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/exception.dart';

/// Evaluation monad for Glue expressions
/// Mirrors Haskell Glue.Eval.Eval exactly

/// The evaluation monad that threads runtime state through computations
class Eval<T> {
  final FutureOr<Either<EvalError, (T, Runtime)>> Function(Runtime) _run;

  const Eval(this._run);

  /// Run the evaluation with initial runtime (matches Haskell runEval)
  FutureOr<Either<EvalError, (T, Runtime)>> runEval(Runtime runtime) =>
      _run(runtime);

  /// Create a successful evaluation
  static Eval<T> pure<T>(T value) => Eval((runtime) => Right((value, runtime)));

  /// Lift an IO operation into the Eval monad
  static Eval<T> liftIO<T>(FutureOr<T> io) => Eval((runtime) async {
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

  /// Map over the result
  Eval<U> map<U>(U Function(T) f) => Eval((runtime) async {
    final result = await runEval(runtime);
    return result.map((tuple) => (f(tuple.$1), tuple.$2));
  });

  /// FlatMap (bind) operation
  Eval<U> flatMap<U>(Eval<U> Function(T) f) => Eval((runtime) async {
    final result = await runEval(runtime);
    return switch (result) {
      Left() => result as Either<EvalError, (U, Runtime)>,
      Right(:final value) => await f(value.$1).runEval(value.$2),
    };
  });

  /// Transform the evaluation result
  Eval<U> transform<U>(
    Either<EvalError, (U, Runtime)> Function(T, Runtime) f,
  ) => Eval((runtime) async {
    final result = await runEval(runtime);
    return switch (result) {
      Left() => result as Either<EvalError, (U, Runtime)>,
      Right(:final value) => f(value.$1, value.$2),
    };
  });
}

/// Convenience alias for IR evaluation
typedef EvalIR = Eval<Ir>;

/// Right-biased Either for results
sealed class Either<L, R> {
  const Either();

  bool get isLeft => this is Left;
  bool get isRight => this is Right;

  T fold<T>(T Function(L) onLeft, T Function(R) onRight) => switch (this) {
    Left(:final value) => onLeft(value),
    Right(:final value) => onRight(value),
  };

  Either<L, R2> map<R2>(R2 Function(R) f) => switch (this) {
    Left(:final value) => Left(value),
    Right(:final value) => Right(f(value)),
  };

  Either<L, R2> flatMap<R2>(Either<L, R2> Function(R) f) => switch (this) {
    Left(:final value) => Left(value),
    Right(:final value) => f(value),
  };
}

class Left<L, R> extends Either<L, R> {
  final L value;
  const Left(this.value);
}

class Right<L, R> extends Either<L, R> {
  final R value;
  const Right(this.value);
}

/// Runtime state access functions

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

/// Throw an evaluation error
Eval<T> throwError<T>(RuntimeException exception) =>
    Eval((runtime) => Left(EvalError(runtime.context, exception)));

/// Define a variable in current environment
Eval<void> defineVarEval(String name, Ir value) => Eval(
  (runtime) =>
      Right(((), runtime.copyWith(env: defineVar(name, value, runtime.env)))),
);

/// Update a variable in current environment
Eval<void> updateVarEval(String name, Ir value) => Eval((runtime) {
  final result = updateVar(name, value, runtime.env);
  return result.fold(
    (error) => Left(EvalError(runtime.context, error)),
    (env) => Right(((), runtime.copyWith(env: env))),
  );
});

/// Run evaluation with temporary environment
Eval<T> withEnv<T>(Env tempEnv, Eval<T> action) => Eval((runtime) async {
  final originalEnv = runtime.env;
  final tempRuntime = runtime.copyWith(env: tempEnv);
  final result = await action.runEval(tempRuntime);
  return result.map((tuple) => (tuple.$1, tuple.$2.copyWith(env: originalEnv)));
});

/// Run evaluation with additional context frame
Eval<T> withContext<T>(String contextName, Eval<T> action) => pushContext(
  contextName,
).flatMap((_) => action.flatMap((value) => popContext().map((_) => value)));

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
/// SIMPLE EVALUATION INTERFACE
/// ============================================================================

/// Simple evaluation with just environment
/// Mirrors Haskell runEvalSimple exactly
/// Returns (result, finalEnv, context) tuple
FutureOr<Either<EvalError, (T, Env, Context)>> runEvalSimple<T>(
  Eval<T> action,
  Env initialEnv,
) async {
  final initialRuntime = Runtime.initial(initialEnv);
  final result = await action.runEval(initialRuntime);
  return result.map((tuple) => (tuple.$1, tuple.$2.env, tuple.$2.context));
}

/// ============================================================================
/// CORE EXPRESSION EVALUATION
/// ============================================================================

/// Main evaluation function - evaluates IR expressions
/// Mirrors Haskell Glue.Eval.eval exactly
EvalIR eval(Ir ir) {
  return switch (ir) {
    IrSymbol(:final value) => evalSymbol(value),
    IrDottedSymbol(:final parts) => evalDottedSymbol(parts),
    IrList(:final elements) => evalList(elements.unlock),
    IrObject(:final properties) => evalObject(properties.unlock),
    // Literals evaluate to themselves
    _ => EvalIR.pure(ir),
  };
}

/// Evaluate a symbol by looking it up in the environment
EvalIR evalSymbol(String name) {
  return getEnv().flatMap((env) {
    final result = lookupVar(name, env);
    return result.fold(
      (error) => throwError(error),
      (value) => EvalIR.pure(value),
    );
  });
}

/// Evaluate dotted symbol access (module.property.field)
EvalIR evalDottedSymbol(List<String> parts) {
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

/// Helper to find the longest prefix that exists
EvalIR _evalWithPrefixes(List<String> parts) {
  return getEnv().flatMap((env) {
    // Try prefixes from longest to shortest
    for (final prefix in _generatePrefixes(parts)) {
      final prefixName = prefix.join('.');
      final result = lookupVar(prefixName, env);

      if (result.isRight) {
        // Found the prefix, now navigate the remaining parts
        return result.fold(
          (error) => throwError(error), // Should not happen
          (value) => _evalNestedAccess(value, parts.sublist(prefix.length)),
        );
      } else {
        // Continue to next prefix
        continue;
      }
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
EvalIR _evalNestedAccess(Ir obj, List<String> remainingParts) {
  if (remainingParts.isEmpty) {
    return EvalIR.pure(obj);
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

/// Evaluate a list (function call or literal list)
EvalIR evalList(List<Ir> elements) {
  if (elements.isEmpty) {
    return EvalIR.pure(IrList([]));
  }

  final first = elements[0];
  final args = elements.sublist(1);

  // If first element is a symbol, it might be a special form or function call
  if (first is IrSymbol) {
    return _evalSymbolCall(first.value, args);
  }

  // Otherwise, evaluate all elements and create a list
  return sequenceAll(
    elements.map(eval).toList(),
  ).map((evaluated) => IrList(evaluated));
}

/// Evaluate a call starting with a symbol
EvalIR _evalSymbolCall(String name, List<Ir> args) {
  return withContext(
    name,
    getEnv().flatMap((env) {
      final result = lookupVar(name, env);

      return result.fold(
        (error) => throwError(error),
        (value) => switch (_isSpecialForm(name)) {
          true => _evalSpecialForm(name, args),
          false => sequenceAll(
            args.map(eval).toList(),
          ).flatMap((evaluatedArgs) => apply(value, evaluatedArgs)),
        },
      );
    }),
  );
}

/// Check if a symbol is a special form
bool _isSpecialForm(String name) {
  return const {
    'def',
    'lambda',
    'λ',
    'let',
    'set',
    'import',
    'quote',
    'if',
    'cond',
  }.contains(name);
}

/// Evaluate special forms (not yet implemented)
EvalIR _evalSpecialForm(String name, List<Ir> args) {
  return throwError(
    RuntimeException(
      'special-form',
      IrString('Special form "$name" not yet implemented'),
    ),
  );
}

/// Evaluate an object
EvalIR evalObject(Map<String, Ir> properties) {
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
EvalIR apply(Ir func, List<Ir> args) {
  return switch (func) {
    IrNative(value: final f) => applyNative(f, args),
    IrClosure(params: final params, body: final body, env: final closureEnv) =>
      applyClosure(params, body, closureEnv, args),
    IrSymbol(value: final name) => throwError(unboundVariable(name)),
    _ => throwError(notCallableObject()),
  };
}

/// Apply a native function/special form
EvalIR applyNative(Native native, List<Ir> args) {
  return switch (native) {
    NativeFunc(function: final f) => _applyNativeFunc(f, args),
    NativeSpecial(function: final s) => s(
      args,
    ), // Special forms handle their own evaluation
  };
}

/// Apply a native function (evaluate arguments first)
EvalIR _applyNativeFunc(dynamic func, List<Ir> rawArgs) {
  return sequenceAll(rawArgs.map(eval).toList()).flatMap((args) => func(args));
}

/// Apply a closure with the given arguments
EvalIR applyClosure(
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

/// Full application of a closure
EvalIR _applyFullClosure(
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
EvalIR _applyPartialClosure(
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
