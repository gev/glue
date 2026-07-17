import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final offsetZero = IrNativeValue(Value(Offset.zero));
final offsetInfinite = IrNativeValue(Value(Offset.infinite));

final offset = IrNativeFunc(offsetImpl);

Eval<Ir> offsetImpl(Ir dxIr) {
  final dx = toDouble(dxIr);
  if (dx == null) {
    return throwError(wrongArgumentType(['dx should be a number']));
  }
  return Eval.pure(
    IrNativeFunc((Ir dyIr) {
      final dy = toDouble(dyIr);
      if (dy == null) {
        return throwError(wrongArgumentType(['dy should be a number']));
      }
      return Eval.pure(IrNativeValue(Value(Offset(dx, dy))));
    }),
  );
}
