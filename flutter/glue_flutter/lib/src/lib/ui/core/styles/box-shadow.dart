import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final Ir boxShadow = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          BoxShadow(
            color: to<Color>(properties['color']) ?? const Color(0xFF000000),
            offset: to<Offset>(properties['offset']) ?? Offset.zero,
            blurRadius: toDouble(properties['blur-radius']) ?? 0.0,
            spreadRadius: toDouble(properties['spread-radius']) ?? 0.0,
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` properties required for box-shadow']),
    ),
  };
});
