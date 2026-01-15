import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Maximum function (returns the larger of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Max.max exactly
Eval<Ir> max(List<Ir> args) {
  return switch (args) {
    [final arg1, final arg2] => sequenceAll([eval(arg1), eval(arg2)]).flatMap((
      values,
    ) {
      final va1 = values[0];
      final va2 = values[1];
      return switch ((va1, va2)) {
        (IrInteger(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrInteger(n1 > n2 ? n1 : n2),
        ),
        (IrFloat(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(n1 > n2 ? n1 : n2),
        ),
        (IrInteger(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(n1 > n2 ? n1.toDouble() : n2),
        ),
        (IrFloat(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrFloat(n1 > n2 ? n1 : n2.toDouble()),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
