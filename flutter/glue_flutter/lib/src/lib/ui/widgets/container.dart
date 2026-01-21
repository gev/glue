import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Container widget function
/// Creates Flutter Container from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object
Eval<Ir> containerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createContainer(
    Properties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Container widget from properties
Eval<Ir> _createContainer(Properties properties) {
  final containerWidget = Container(
    padding: properties.padding,
    color: properties.color,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
}
