import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Arcsine function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asin exactly
final Ir asin = IrNativeFunc(asinImpl);

/// Arcsine function implementation (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asinImpl exactly
Eval<Ir> asinImpl(Ir arg) {
  return eval(arg).flatMap((va) {
    return switch (va) {
      IrInteger(value: final n) => Eval.pure(IrFloat(math.asin(n.toDouble()))),
      IrFloat(value: final n) => Eval.pure(IrFloat(math.asin(n))),
      _ => throwError(wrongArgumentType(['number'])),
    };
  });
}
