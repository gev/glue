import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

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
