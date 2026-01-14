import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Let special form - creates a new scope with local bindings
/// Mirrors Haskell Glue.Lib.Builtin.Let.let' exactly
Eval<Ir> let(List<Ir> args) {
  if (args.length != 2) {
    return throwError(wrongArgumentType(['object', 'body']));
  }

  final bindings = args[0];
  final body = args[1];

  if (bindings is! IrObject) {
    return throwError(wrongArgumentType(['object', 'body']));
  }

  // Evaluate all binding values and create new frame
  final bindingPairs = <(String, Ir)>[];

  return _evalBindings(bindings.properties.unlock, bindingPairs).flatMap((
    evaluatedPairs,
  ) {
    // Push new frame with bindings onto current environment
    return getEnv().flatMap((currentEnv) {
      final newFrame = frameFromList(
        evaluatedPairs.map((pair) => (pair.$1, pair.$2)).toList(),
      );

      final newEnv = currentEnv.add(newFrame);

      // Evaluate body in extended environment
      return putEnv(newEnv).flatMap((_) {
        return eval(body).flatMap((result) {
          // Pop the frame
          return putEnv(currentEnv).map((_) => result);
        });
      });
    });
  });
}

/// Helper to evaluate all bindings recursively
Eval<List<(String, Ir)>> _evalBindings(
  Map<String, Ir> bindings,
  List<(String, Ir)> accumulator,
) {
  if (bindings.isEmpty) {
    return Eval.pure(accumulator);
  }

  final entry = bindings.entries.first;
  final remaining = Map<String, Ir>.from(bindings)..remove(entry.key);

  return eval(entry.value).flatMap((evaluatedValue) {
    accumulator.add((entry.key, evaluatedValue));
    return _evalBindings(remaining, accumulator);
  });
}
