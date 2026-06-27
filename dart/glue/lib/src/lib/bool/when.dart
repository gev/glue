import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// When conditional execution special form
/// Mirrors Haskell Glue.Lib.Bool.When.when_ exactly
final Ir when_ = IrSpecial(whenImpl);

/// When conditional execution special form implementation
/// Mirrors Haskell Glue.Lib.Bool.When.whenImpl exactly
Eval<Ir> whenImpl(List<Ir> args) {
  return switch (args) {
    [final cond, final body] => eval(cond).bind((condVal) {
      return isTruthy(condVal) ? eval(body) : Eval.pure(IrVoid());
    }),
    _ => throwError(wrongArgumentType(['condition', 'body'])),
  };
}
