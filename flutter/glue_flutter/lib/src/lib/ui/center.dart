import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center child) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes child
Eval<Ir> centerImpl(Ir child) => switch (child) {
  IrNativeValue(value: HostValue(value: Widget childWidget)) => Eval.pure(
    IrNativeValue(HostValue(Center(child: childWidget))),
  ),
  _ => throwError(wrongArgumentType(['widget'])),
};
