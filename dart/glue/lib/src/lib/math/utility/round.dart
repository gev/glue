import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Round function (rounds to nearest integer)
/// Mirrors Haskell Glue.Lib.Math.Utility.Round.round exactly
Eval<Ir> round(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.round())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
