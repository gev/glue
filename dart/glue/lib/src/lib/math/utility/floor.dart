import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Floor function (rounds down to nearest integer)
/// Mirrors Haskell Glue.Lib.Math.Utility.Floor.floor exactly
Eval<Ir> floor(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrInteger(n)),
        IrFloat(value: final n) => Eval.pure(IrInteger(n.floor())),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
