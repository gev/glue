import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Tangent function (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Tan.tan exactly
Eval<Ir> tan(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrFloat(math.tan(n.toDouble()))),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.tan(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
