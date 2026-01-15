import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Truncate function (removes decimal part)
/// Mirrors Haskell Glue.Lib.Math.Utility.Trunc.trunc exactly
Eval<Ir> trunc(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.truncate())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
