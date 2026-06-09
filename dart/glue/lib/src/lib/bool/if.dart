import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// If special form
/// Mirrors Haskell Glue.Lib.Bool.If.if_ exactly
final Ir if_ = IrSpecial(ifImpl);

/// If special form implementation
/// Mirrors Haskell Glue.Lib.Bool.If.ifImpl exactly
Eval<Ir> ifImpl(List<Ir> args) {
  return switch (args) {
    [final cond, final thenExpr, final elseExpr] => eval(cond).bind((condVal) {
      return switch (condVal) {
        IrBool(value: false) => eval(elseExpr),
        _ => eval(thenExpr),
      };
    }),
    _ => throwError(wrongArgumentType(['condition', 'then', 'else'])),
  };
}
