import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

/// Square root function
/// Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrt exactly
Eval<Ir> sqrt(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(
          IrFloat(math.sqrt(n.toDouble())),
        ),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.sqrt(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
