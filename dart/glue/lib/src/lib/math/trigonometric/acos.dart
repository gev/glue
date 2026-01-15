import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Arccosine function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Acos.acos exactly
Eval<Ir> acos(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(
          IrFloat(math.acos(n.toDouble())),
        ),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.acos(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
