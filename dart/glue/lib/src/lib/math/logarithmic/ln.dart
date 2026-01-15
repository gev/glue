import 'dart:math' as math;

import 'package:glue/src/../eval.dart';
import 'package:glue/src/../eval/exception.dart';
import 'package:glue/src/../ir.dart';

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
