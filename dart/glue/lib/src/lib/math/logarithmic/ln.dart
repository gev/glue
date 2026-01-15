import 'dart:math' as math;

import '../../../eval.dart';
import '../../../eval/exception.dart';
import '../../../ir.dart';

/// Natural logarithm function (base e)
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Ln.ln exactly
Eval<Ir> ln(List<Ir> args) {
  return switch (args) {
    [final arg] => eval(arg).flatMap((va) {
      return switch (va) {
        IrInteger(value: final n) => Eval.pure(IrFloat(math.log(n.toDouble()))),
        IrFloat(value: final n) => Eval.pure(IrFloat(math.log(n))),
        _ => throwError(wrongArgumentType(['number'])),
      };
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
