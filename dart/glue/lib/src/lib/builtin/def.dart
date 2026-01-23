import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
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
    // Variable definition: (def symbol value)
    [IrSymbol(:final value), final rawVal] => eval(rawVal).flatMap((evaluated) {
      return defineVarEval(value, evaluated).map((_) => IrVoid());
    }),

    // Function definition sugar: (def (symbol params...) body...)
    [IrList(:final elements), ...final body] when elements.isNotEmpty =>
      switch (elements[0]) {
        IrSymbol(:final value) =>
          extractSymbols(elements.sublist(1).unlock).match(
            (_) => throwError(
              wrongArgumentType(['symbols in function parameters']),
            ),
            (paramSymbols) {
              // Create body expression - mirrors Haskell exactly
              final bodyExpr = switch (body) {
                [] => IrVoid(),
                [final single] => single,
                final multiple => IrList(multiple),
              };

              // Create closure and define it
              return makeClosure(paramSymbols, bodyExpr).flatMap((closure) {
                return defineVarEval(value, closure).map((_) => IrVoid());
              });
            },
          ),
        _ => throwError(wrongArgumentType(['function name symbol'])),
      },

    // Invalid arguments
    _ => throwError(
      wrongArgumentType(['symbol or function signature', 'value']),
    ),
  };
}
