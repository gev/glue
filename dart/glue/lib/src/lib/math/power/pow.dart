import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Power function (base^exponent)
/// Mirrors Haskell Glue.Lib.Math.Power.Pow.pow exactly
Eval<Ir> pow(List<Ir> args) {
  return switch (args) {
    [final arg1, final arg2] => sequenceAll([eval(arg1), eval(arg2)]).flatMap((
      values,
    ) {
      final va1 = values[0];
      final va2 = values[1];
      return switch ((va1, va2)) {
        (IrInteger(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrInteger(math.pow(n1, n2).toInt()),
        ),
        (IrInteger(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(math.pow(n1.toDouble(), n2).toDouble()),
        ),
        (IrFloat(value: final n1), IrInteger(value: final n2)) => Eval.pure(
          IrFloat(math.pow(n1, n2.toDouble()).toDouble()),
        ),
        (IrFloat(value: final n1), IrFloat(value: final n2)) => Eval.pure(
          IrFloat(math.pow(n1, n2).toDouble()),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
