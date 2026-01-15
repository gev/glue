import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Exponential function (e^x)
/// Mirrors Haskell Glue.Lib.Math.Power.Exp.exp exactly
Eval<Ir> exp(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrFloat(math.exp(n.toDouble()))),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.exp(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
