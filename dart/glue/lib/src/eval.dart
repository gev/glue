import 'package:glue/src/either.dart';
import 'package:glue/src/env.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module/cache.dart';
import 'package:glue/src/module/registry.dart';
import 'package:glue/src/runtime.dart';

sealed class Trampoline<T> {}

class Done<T> extends Trampoline<T> {
  final Either<EvalError, (T, Runtime)> result;
  Done(this.result);
}

class Suspend<T> extends Trampoline<T> {
  final Eval<T> nextEval;
  final Runtime runtime;
  Suspend(this.nextEval, this.runtime);
}

/// Evaluation monad for Glue expressions
/// Mirrors Haskell Glue.Eval.Eval exactly
// class Eval<T> {
//   final Either<EvalError, (T, Runtime)> Function(Runtime) _run;

//   const Eval(this._run);

//   /// Create a successful evaluation
//   static Eval<T> pure<T>(T value) => Eval((runtime) => Right((value, runtime)));

//   /// Map over the result
//   Eval<U> map<U>(U Function(T) f) => bind((value) => Eval.pure(f(value)));

//   /// FlatMap (bind) operation
//   Eval<U> bind<U>(Eval<U> Function(T) f) => Eval((runtime) {
//     final result = runEval(this, runtime);
//     return result.match((error) => Left(error), (value) {
//       final (result, runtime) = value;
//       return runEval(f(result), runtime);
//     });
//   });
// }

class Eval<T> {
  final Trampoline<T> Function(Runtime) _run;

  const Eval(this._run);

  /// Базовое значение возвращает Done мгновенно
  static Eval<T> pure<T>(T value) =>
      Eval((runtime) => Done(Right((value, runtime))));

  /// Ошибка тоже возвращает Done
  static Eval<T> fail<T>(EvalError error) =>
      Eval((runtime) => Done(Left(error)));

  /// Функция ручного прерывания (чтобы разбить тяжелые вычисления)
  static Eval<T> defer<T>(Eval<T> Function() thunk) =>
      Eval((runtime) => Suspend(thunk(), runtime));

  /// Map over the result (Optimized for Trampoline)
  Eval<U> map<U>(U Function(T) f) => Eval((runtime) {
    final step = _run(runtime);

    return switch (step) {
      // 1. Если текущий шаг вернул результат, мы просто применяем
      // функцию к значению и сразу отдаем Done. Никаких лишних Suspend!
      Done(:final result) => Done(
        result.match(
          (error) => Left(error), // Ошибку не трогаем
          (value) {
            final (t, newRuntime) = value;
            return Right((f(t), newRuntime)); // Меняем T на U
          },
        ),
      ),

      // 2. Если вычисление ушло в паузу, мы берем следующий Eval
      // и говорим ему: "Когда выполнишься, не забудь сделать map(f)".
      Suspend(:final nextEval, :final runtime) => Suspend(
        nextEval.map(f),
        runtime,
      ),
    };
  });

  /// ТОТ САМЫЙ BIND, КОТОРЫЙ НЕ ПАДАЕТ
  Eval<U> bind<U>(Eval<U> Function(T) f) => Eval((runtime) {
    // 1. Делаем ровно ОДИН шаг текущего вычисления
    final step = _run(runtime);

    return switch (step) {
      // 2. Если левая часть завершилась...
      Done(:final result) => result.match(
        // Ошибку прокидываем дальше
        (error) => Done(Left(error)),
        (value) {
          final (t, newRuntime) = value;
          // УСПЕХ: Мы применяем функцию `f`, получаем следующий Eval,
          // НО НЕ ЗАПУСКАЕМ ЕГО! Мы возвращаем Suspend.
          // Это заставит Dart ВЫЙТИ из функции и отдать Eval в while-цикл.
          return Suspend(f(t), newRuntime);
        },
      ),

      // 3. Если левая часть ЕЩЕ НЕ завершилась (внутри глубокая рекурсия)...
      Suspend(:final nextEval, :final runtime) =>
        // Мы берем следующий шаг и "приклеиваем" к нему наш bind!
        // Dart-стек тут не растет, мы просто создаем новый объект Eval в куче.
        Suspend(nextEval.bind(f), runtime),
    };
  });
}

/// ============================================================================
/// BASIC EVALUATION API
/// ============================================================================

/// Run the evaluation with initial runtime (matches Haskell runEval)
// Either<EvalError, (T, Runtime)> runEval<T>(Eval<T> eval, Runtime runtime) =>
//     eval._run(runtime);

Either<EvalError, (T, Runtime)> runEval<T>(Eval<T> eval, Runtime runtime) {
  // Кладем первую задачу в "луп"
  Trampoline<T> current = Suspend(eval, runtime);

  while (true) {
    switch (current) {
      // Событие: всё закончилось. Прерываем цикл и отдаем ответ.
      case Done(:final result):
        return result;

      // Событие: пауза.
      case Suspend(:final nextEval, :final runtime):
        // Выполняем ОДИН шаг и обновляем current.
        // Так как nextEval._run возвращает Trampoline,
        // мы никогда не уходим в стек вызовов глубже 1 уровня!
        current = nextEval._run(runtime);
        break;
    }
  }
}

/// Simple evaluation with just environment
/// Mirrors Haskell runEvalSimple exactly
Either<EvalError, (T, Runtime)> runEvalSimple<T>(
  Eval<T> action,
  Env initialEnv,
) {
  final initialRuntime = Runtime.initial(initialEnv);
  return runEval(action, initialRuntime);
}

/// Throw an evaluation error
Eval<T> throwError<T>(RuntimeException exception) =>
    Eval((runtime) => Done(Left(EvalError(runtime.stack, exception))));

/// ============================================================================
/// ENVIRONMENT AND RUNTIME ACCESS
/// ============================================================================

/// Get current environment
Eval<Env> getEnv() => Eval((runtime) => Done(Right((runtime.env, runtime))));

/// Set current environment
Eval<void> putEnv(Env env) =>
    Eval((runtime) => Done(Right(((), runtime.copyWith(env: env)))));

/// Get root environment
Eval<Env> getRootEnv() =>
    Eval((runtime) => Done(Right((runtime.rootEnv, runtime))));

/// Set root environment
Eval<void> putRootEnv(Env rootEnv) =>
    Eval((runtime) => Done(Right(((), runtime.copyWith(rootEnv: rootEnv)))));

/// Get current stack
Eval<CallStack> getStack() =>
    Eval((runtime) => Done(Right((runtime.stack, runtime))));

/// Push stack frame
Eval<void> pushCall(String name) => Eval(
  (runtime) =>
      Done(Right(((), runtime.copyWith(stack: [name, ...runtime.stack])))),
);

/// Pop stack frame
Eval<void> popCall() => Eval(
  (runtime) => Done(
    runtime.stack.isEmpty
        ? Left(
            EvalError(
              runtime.stack,
              RuntimeException(
                'stack-error',
                IrString('Cannot pop empty stack'),
              ),
            ),
          )
        : Right(((), runtime.copyWith(stack: runtime.stack.sublist(1)))),
  ),
);

/// Get module registry
Eval<ModuleRegistry> getRegistry() =>
    Eval((runtime) => Done(Right((runtime.registry, runtime))));

/// Get import cache
Eval<ImportedModuleCache> getCache() =>
    Eval((runtime) => Done(Right((runtime.importCache, runtime))));

// /// Set import cache
// Eval<void> putCache(ImportedModuleCache cache) =>
//     Eval((runtime) => Right(((), runtime.copyWith(importCache: cache))));

/// Get complete runtime
Eval<Runtime> getRuntime() =>
    Eval((runtime) => Done(Right((runtime, runtime))));

/// Set complete runtime
Eval<void> putRuntime(Runtime newRuntime) =>
    Eval((_) => Done(Right(((), newRuntime))));

/// ============================================================================
/// VARIABLE MANAGEMENT
/// ============================================================================

/// Define a variable in current environment
Eval<void> defineVarEval(String name, Ir value) => Eval(
  (runtime) => Done(
    Right(((), runtime.copyWith(env: defineVar(name, value, runtime.env)))),
  ),
);

/// ============================================================================
/// ENVIRONMENT UTILITIES
/// ============================================================================

/// Run evaluation with temporary environment
Eval<T> withEnv<T>(Env tempEnv, Eval<T> action) => Eval((runtime) {
  final originalEnv = runtime.env;
  final tempRuntime = runtime.copyWith(env: tempEnv);
  final result = runEval(action, tempRuntime);
  return result.match((error) => Done(Left(error)), (value) {
    final (result, runtime) = value;
    return Done(Right((result, runtime.copyWith(env: originalEnv))));
  });
});

/// Run evaluation with additional stack frame
Eval<T> withCall<T>(String contextName, Eval<T> action) => pushCall(
  contextName,
).bind((_) => action.bind((value) => popCall().map((_) => value)));

/// ============================================================================
/// EVALUATION SEQUENCING
/// ============================================================================

/// Sequence two evaluations
Eval<(T1, T2)> sequence<T1, T2>(Eval<T1> first, Eval<T2> second) =>
    first.bind((a) => second.map((b) => (a, b)));

/// Sequence multiple evaluations
Eval<List<T>> sequenceAll<T>(List<Eval<T>> evals) {
  if (evals.isEmpty) return Eval.pure([]);

  return evals[0].bind(
    (first) => sequenceAll(evals.sublist(1)).map((rest) => [first, ...rest]),
  );
}

/// Evaluate multiple expressions and return the last result
Eval<T> sequence_<T>(List<Eval<dynamic>> evals, Eval<T> last) {
  if (evals.isEmpty) return last;

  return evals[0].bind((_) => sequence_(evals.sublist(1), last));
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

/// Evaluate function body with implicit sequence semantics
/// Mirrors Haskell Glue.Eval.evalBody exactly
Eval<Ir> evalBody(List<Ir> body) {
  return sequenceAll(body.map(eval).toList()).bind((result) {
    return switch (result) {
      [] => Eval.pure(IrVoid()),
      _ => Eval.pure(result.last),
    };
  });
}

/// Evaluate a symbol by looking it up in the environment
Eval<Ir> evalSymbol(String name) {
  return getEnv().bind((env) {
    final result = lookupVar(name, env);
    return result.match(
      (error) => throwError(error),
      (value) => switch (value) {
        IrEvaluable(:final func) => withCall(name, func().bind(Eval.pure)),
        _ => Eval.pure(value),
      },
    );
  });
}

/// Evaluate dotted symbol access (base.property.field)
Eval<Ir> evalDottedSymbol(List<String> parts) {
  return switch (parts) {
    [] => throwError(
      RuntimeException('invalid-symbol', IrString('Empty dotted symbol')),
    ),
    [final base] => evalSymbol(base),
    [final base, ...final rest] => evalSymbol(base).bind((value) {
      return _evalNestedAccess(value, rest);
    }),
  };
}

/// Evaluate a list (function call or literal list)
/// Mirrors Haskell evalList exactly using pattern matching
Eval<Ir> evalList(List<Ir> elements) {
  return switch (elements) {
    // Pattern: [IR.Symbol name]
    [IrSymbol(value: final name)] => evalSymbol(name),

    // Pattern: [IR.DottedSymbol parts]
    [IrDottedSymbol(:final parts)] => evalDottedSymbol(parts),

    // Pattern: (IR.Symbol name : rawArgs)
    [IrSymbol(value: final name), ...final rawArgs] => withCall(
      name,
      getEnv().bind((env) {
        final result = lookupVar(name, env);
        return result.match(
          (error) => throwError(error),
          (value) => apply(value, rawArgs),
        );
      }),
    ),

    // Pattern: [IR.DottedSymbol parts : rawArgs]
    [IrDottedSymbol(:final parts), ...final rawArgs] => evalDottedSymbol(
      parts,
    ).bind((func) => withCall(parts.join('.'), apply(func, rawArgs))),

    // Pattern: xs (other lists)
    _ => withCall(
      '<call>',
      sequenceAll(elements.map(eval).toList()).bind((evaluated) {
        return switch (evaluated) {
          [final f, ...final args] when isCallable(f) => apply(f, args),
          _ => Eval.pure(IrList(evaluated)),
        };
      }),
    ),
  };
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

/// Apply a function to arguments (universal currying)
/// Mirrors Haskell Glue.Eval.apply exactly
Eval<Ir> apply(Ir func, List<Ir> args) {
  return switch (func) {
    IrNativeFunc(function: final f) => _applyNativeFunc(f, args),
    IrSpecial(function: final s) => s(args),
    IrClosure(params: final params, body: final body, env: final closureEnv) =>
      applyClosure(params, body, closureEnv, args),
    _ => throwError(notCallableObject()),
  };
}

/// Apply a native function with universal currying
/// Mirrors Haskell applyNativeFunc exactly
Eval<Ir> _applyNativeFunc(Eval<Ir> Function(Ir) func, List<Ir> args) {
  return switch (args) {
    [] => Eval.pure(
      IrNativeFunc(func),
    ), // No args, return function as-is (like Haskell)
    [final first, ...final rest] => eval(first).bind((arg) {
      return func(arg).bind((result) {
        return switch (isCallable(result)) {
          true => apply(result, rest), // Apply remaining args to result
          false => switch (rest) {
            [] => Eval.pure(result), // No more args, return result
            _ => throwError(wrongNumberOfArguments()), // Too many args
          },
        };
      });
    }),
  };
}

/// Apply a closure with the given arguments
Eval<Ir> applyClosure(
  List<String> params,
  List<Ir> body,
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
          : Eval.pure(IrVoid()),

    IrNativeValue(value: final hostValue) =>
      // Handle property access on host values (FFI)
      hostValue.getters[prop] != null
          ? hostValue.getters[prop]!.bind(
              (result) => _evalNestedAccess(result, rest),
            )
          : Eval.pure(IrVoid()),

    _ => throwError(notAnObject(obj)),
  };
}

/// Check if an IR value can be called
/// Mirrors Haskell Glue.Eval.isCallable exactly
bool isCallable(Ir value) {
  return switch (value) {
    IrNativeFunc() => true,
    IrSpecial() => true,
    IrClosure() => true,
    _ => false,
  };
}

/// Full application of a closure
Eval<Ir> _applyFullClosure(
  List<String> params,
  List<Ir> body,
  Env closureEnv,
  List<Ir> rawArgs,
) {
  return sequenceAll(rawArgs.map(eval).toList()).bind((args) {
    final bindings = <(String, Ir)>[];
    for (var i = 0; i < params.length; i++) {
      bindings.add((params[i], args[i]));
    }
    return withEnv(_buildEnvWithBindings(closureEnv, bindings), evalBody(body));
  });
}

/// Partial application of a closure
Eval<Ir> _applyPartialClosure(
  List<String> params,
  List<Ir> body,
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
