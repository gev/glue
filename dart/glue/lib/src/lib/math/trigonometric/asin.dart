import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Arcsine function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asin exactly
Eval<Ir> asin(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(
          IrFloat(math.asin(n.toDouble())),
        ),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.asin(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
