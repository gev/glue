import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Container widget function
/// Creates Flutter Container from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object
Eval<Ir> containerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createContainer(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Container widget from properties
Eval<Ir> _createContainer(CoreProperties properties) {
  final containerWidget = Container(
    alignment: properties.alignment,
    padding: properties.padding,
    color: properties.color,
    decoration: properties.decoration,
    foregroundDecoration: properties.foregroundDecoration,
    width: properties.width,
    height: properties.height,
    constraints: properties.constraints,
    margin: properties.margin,
    transform: properties.transform,
    transformAlignment: properties.transformAlignment,
    child: properties.child,
    clipBehavior: properties.clipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(containerWidget)));
}
