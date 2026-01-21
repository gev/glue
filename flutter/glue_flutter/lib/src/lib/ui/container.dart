import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Container widget function
/// Creates Flutter Container from Glue (container child props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes child widget
Eval<Ir> containerImpl(Ir child) {
  return Eval.pure(IrNativeFunc(containerWithChild(child)));
}

/// Container with child - takes optional properties
Eval<Ir> Function(Ir) containerWithChild(Ir child) =>
    (Ir props) => switch ((child, props)) {
      (
        IrNativeValue(value: HostValue(value: Widget childWidget)),
        IrObject(:final properties),
      ) =>
        _createContainer(childWidget, Properties(properties.unlock)),
      (IrNativeValue(value: HostValue(value: Widget childWidget)), _) =>
        _createContainer(childWidget, Properties.empty()),
      _ => throwError(wrongArgumentType(['widget', 'object?'])),
    };

/// Create Container widget from child and properties
Eval<Ir> _createContainer(Widget child, Properties properties) {
  final containerWidget = Container(
    padding: properties.padding,
    color: properties.color,
    child: child,
  );
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
}
