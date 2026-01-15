import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Arctangent function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Atan.atan exactly
Eval<Ir> atan(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(
          IrFloat(math.atan(n.toDouble())),
        ),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.atan(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
