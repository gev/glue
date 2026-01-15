import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Logarithm base 10 function
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Lg.lg exactly
Eval<Ir> lg(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(
          IrFloat(math.log(n.toDouble()) / math.ln10),
        ),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.log(n) / math.ln10)),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
