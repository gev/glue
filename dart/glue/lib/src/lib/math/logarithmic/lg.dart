import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Logarithm base 10 function
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Lg.lg exactly
final Ir lg = IrNativeFunc(lgImpl);

/// Logarithm base 10 function implementation
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Lg.lgImpl exactly
Eval<Ir> lgImpl(Ir arg) {
  return switch (arg) {
    IrInteger(value: final n) => Eval.pure(
      IrFloat(math.log(n.toDouble()) / math.ln10),
    ),
    IrFloat(value: final n) => Eval.pure(IrFloat(math.log(n) / math.ln10)),
    _ => throwError(wrongArgumentType(['number'])),
  };
}
