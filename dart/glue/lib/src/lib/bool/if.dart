import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// If conditional special form
/// Mirrors Haskell Glue.Lib.Bool.If.if_ exactly
Eval<Ir> if_(List<Ir> args) {
  return switch (args) {
    [final cond, final thenExpr, final elseExpr] => eval(cond).flatMap((
      condVal,
    ) {
      return switch (condVal) {
        IrBool(value: false) => eval(elseExpr),
        _ => eval(thenExpr),
      };
    }),
    _ => throwError(wrongArgumentType(['condition', 'then', 'else'])),
  };
}
