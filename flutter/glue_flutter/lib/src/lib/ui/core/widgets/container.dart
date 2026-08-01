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
    key: properties.key,
    alignment: properties.getValue<Alignment>('alignment'),
    padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    color: properties.getColor('color'),
    decoration: properties.getValue<Decoration>('decoration'),
    foregroundDecoration: properties.getValue<Decoration>(
      'foreground-decoration',
    ),
    width: properties.width,
    height: properties.height,
    constraints: properties.getValue<BoxConstraints>('constraints'),
    margin: properties.getValue<EdgeInsetsGeometry>('margin'),
    transform: properties.getValue<Matrix4>('transform'),
    transformAlignment: properties.getValue<Alignment>('transform-alignment'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(containerWidget)));
}
