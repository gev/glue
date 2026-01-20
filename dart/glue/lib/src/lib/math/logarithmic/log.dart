import 'dart:math' as math;

import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Logarithm with arbitrary base function
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.log exactly
final Ir log = IrNativeFunc(logImpl);

/// Logarithm with arbitrary base function implementation
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.logImpl exactly
Eval<Ir> logImpl(Ir value) {
  return Eval.pure(IrNativeFunc(logWithBase(value)));
}

/// Helper function for base argument
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.logWithBase exactly
Eval<Ir> Function(Ir) logWithBase(Ir value) {
  return (Ir base) {
    return sequenceAll([eval(value), eval(base)]).flatMap((values) {
      return switch (values) {
        [IrInteger(value: final n), IrInteger(value: final b)] => Eval.pure(
          IrFloat(math.log(n.toDouble()) / math.log(b.toDouble())),
        ),
        [IrInteger(value: final n), IrFloat(value: final b)] => Eval.pure(
          IrFloat(math.log(n.toDouble()) / math.log(b)),
        ),
        [IrFloat(value: final n), IrInteger(value: final b)] => Eval.pure(
          IrFloat(math.log(n) / math.log(b.toDouble())),
        ),
        [IrFloat(value: final n), IrFloat(value: final b)] => Eval.pure(
          IrFloat(math.log(n) / math.log(b)),
        ),
        _ => throwError(wrongArgumentType(['number', 'number'])),
      };
    });
  };
}
