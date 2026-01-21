import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_center.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center child) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes child
Eval<Ir> centerImpl(Ir child) {
  if (child is! IrNativeValue) {
    return throwError(wrongArgumentType(['widget']));
  }

  // Extract the child widget from IrNativeValue
  final hostValue = child.value;
  if (hostValue.value is! Widget) {
    return throwError(wrongArgumentType(['widget']));
  }

  final childWidget = hostValue.value as Widget;
  final centerWidget = GlueCenter(child: childWidget);
  return Eval.pure(IrNativeValue(HostValue(centerWidget)));
}
