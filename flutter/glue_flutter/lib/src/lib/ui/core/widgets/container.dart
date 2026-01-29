import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Container widget function
/// Creates Flutter Container from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object
Eval<Ir> containerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createContainer(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Container widget from properties
Eval<Ir> _createContainer(WidgetProperties properties) {
  final containerWidget = Container(
    alignment: properties.getValue('alignment'),
    padding: properties.getValue('padding'),
    color: properties.getColor('color'),
    decoration: properties.getValue('decoration'),
    foregroundDecoration: properties.getValue('foreground-decoration'),
    width: properties.width,
    height: properties.height,
    constraints: properties.getValue('constraints'),
    margin: properties.getValue('margin'),
    transform: properties.getValue('transform'),
    transformAlignment: properties.getValue('transformAlignment'),
    clipBehavior: properties.getValue('clip-behavior'),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(containerWidget)));
}
