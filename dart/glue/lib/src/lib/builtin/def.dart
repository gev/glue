import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/builtin/lambda.dart'
    show extractSymbols, makeClosure;

/// Def special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Def exactly

/// Def special form - defines variables and functions
/// Mirrors Haskell Glue.Lib.Builtin.Def.def exactly
final Ir def = IrSpecial(defImpl);

/// Def special form implementation
/// Mirrors Haskell Glue.Lib.Builtin.Def.defImpl exactly
Eval<Ir> defImpl(List<Ir> args) {
  return switch (args) {
    [IrSymbol(value: final name), final value] => eval(
      value,
    ).bind((evaluated) => defineVarEval(name, evaluated).map((_) => IrVoid())),

    [IrList(elements: final elements), ...final body] => switch (elements
        .unlock) {
      [IrSymbol(value: final name), ...final params] =>
        extractSymbols(params).match(
          (_) =>
              throwError(wrongArgumentType(['symbols in function parameters'])),
          (paramNames) {
            // Create closure and define it
            return makeClosure(paramNames, body).bind(
              (closure) => defineVarEval(name, closure).map((_) => IrVoid()),
            );
          },
        ),
      _ => throwError(wrongArgumentType(['function name symbol'])),
    },

    _ => throwError(
      wrongArgumentType(['symbol or function signature', 'value']),
    ),
  };
}
