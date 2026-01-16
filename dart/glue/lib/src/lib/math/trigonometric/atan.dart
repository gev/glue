import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

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
