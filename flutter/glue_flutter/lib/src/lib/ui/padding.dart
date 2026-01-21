import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding widget function
/// Creates Flutter Padding from Glue (padding child props) expressions
final Ir padding = IrNativeFunc(paddingImpl);

/// Padding implementation - takes child, then properties
Eval<Ir> paddingImpl(Ir child) {
  return Eval.pure(IrNativeFunc(paddingWithChild(child)));
}

/// Padding with child - takes properties object
Eval<Ir> Function(Ir) paddingWithChild(Ir child) =>
    (Ir props) => switch (child) {
      IrNativeValue(value: HostValue(value: Widget childWidget))
          when props is IrObject =>
        () {
          final properties = props.properties.unlock as Map<String, dynamic>;
          final padding =
              extractEdgeInsets(properties['padding']) ?? EdgeInsets.zero;

          final paddingWidget = Padding(
            padding: padding ?? EdgeInsets.zero,
            child: childWidget,
          );
          return Eval.pure(IrNativeValue(HostValue(paddingWidget)));
        }(),
      _ => throwError(wrongArgumentType(['widget', 'object'])),
    };
