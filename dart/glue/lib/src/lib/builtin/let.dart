import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Let special form - creates a new scope with sequential definitions and body expressions.
/// Usage: (let (def foo 1) (def bar (+ foo 1)) bar)
final Ir let = IrSpecial(letImpl);

/// Let special form implementation
Eval<Ir> letImpl(List<Ir> args) {
  if (args.isEmpty) {
    return throwError(wrongArgumentType(['body']));
  }
  return getEnv().bind((currentEnv) {
    final localEnv = currentEnv.add(frameFromList([]));
    return withEnv(localEnv, evalBody(args));
  });
}
