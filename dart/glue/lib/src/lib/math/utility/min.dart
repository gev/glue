import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Minimum function (returns the smaller of two numbers)
/// Mirrors Haskell Glue.Lib.Math.Utility.Min.min exactly
Eval<Ir> min(List<Ir> args) {
  return switch (args) {
    [final arg1, final arg2] => sequenceAll([eval(arg1), eval(arg2)]).flatMap((
      values,
    ) {
      final va1 = values[0];
      final va2 = values[1];
      return switch ((va1, va2)) {
        (IrInteger(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrInteger(n1 < n2 ? n1 : n2),
        ),
        (IrFloat(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(n1 < n2 ? n1 : n2),
        ),
        (IrInteger(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(n1 < n2 ? n1.toDouble() : n2),
        ),
        (IrFloat(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrFloat(n1 < n2 ? n1 : n2.toDouble()),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
