import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

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
