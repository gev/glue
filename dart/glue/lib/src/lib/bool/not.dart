import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Logical not function
/// Mirrors Haskell Glue.Lib.Bool.Not.not_ exactly
Eval<Ir> not(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((value) {
      return switch (value) {
        IrBool(value: false) => Eval.pure(IrBool(true)),
        _ => Eval.pure(IrBool(false)),
      };
    }),
    _ => throwError(wrongArgumentType(['arg'])),
  };
}
