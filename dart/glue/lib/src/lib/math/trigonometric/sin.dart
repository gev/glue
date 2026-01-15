import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Sine function (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Sin.sin exactly
Eval<Ir> sin(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrFloat(math.sin(n.toDouble()))),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.sin(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
