import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

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
