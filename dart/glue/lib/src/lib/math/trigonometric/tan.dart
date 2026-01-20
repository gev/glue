import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Tangent function (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Tan.tan exactly
final Ir tan = IrNativeFunc(tanImpl);

/// Tangent function implementation (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.Tan.tanImpl exactly
Eval<Ir> tanImpl(Ir arg) {
  return eval(arg).flatMap((va) {
    return switch (va) {
      IrInteger(value: final n) => Eval.pure(IrFloat(math.tan(n.toDouble()))),
      IrFloat(value: final n) => Eval.pure(IrFloat(math.tan(n))),
      _ => throwError(wrongArgumentType(['number'])),
    };
  });
}
