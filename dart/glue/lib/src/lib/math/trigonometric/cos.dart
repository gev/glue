import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Cosine function (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Cos.cos exactly
Eval<Ir> cos(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrFloat(math.cos(n.toDouble()))),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.cos(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
