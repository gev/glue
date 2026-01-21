import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Arctangent function (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Atan.atan exactly
final Ir atan = IrNativeFunc(atanImpl);

/// Arctangent function implementation (returns radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Atan.atanImpl exactly
Eval<Ir> atanImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(IrFloat(math.atan(n.toDouble()))),
    IrFloat(value: final n) => Eval.pure(IrFloat(math.atan(n))),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
