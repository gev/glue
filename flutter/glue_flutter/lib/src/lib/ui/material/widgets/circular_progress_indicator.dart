import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CircularProgressIndicator widget function
/// Creates Flutter CircularProgressIndicator from Glue (circular-progress-indicator props) expressions
final Ir circularProgressIndicator = IrNativeFunc(
  circularProgressIndicatorImpl,
);

/// CircularProgressIndicator implementation - takes properties object
Eval<Ir> circularProgressIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCircularProgressIndicator(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCircularProgressIndicator(WidgetProperties.empty()),
};

/// Create CircularProgressIndicator widget from properties
Eval<Ir> _createCircularProgressIndicator(WidgetProperties properties) {
  final circularProgressIndicatorWidget = CircularProgressIndicator(
    key: properties.key,
    value: properties.getDouble('value'),
    backgroundColor: properties.getColor('background-color'),
    color: properties.getColor('color'),
    strokeWidth: properties.getDouble('stroke-width'),
    strokeAlign: properties.getDouble('stroke-align'),
    strokeCap: properties.getValue<StrokeCap>('stroke-cap'),
    semanticsLabel: properties.getString('semantics-label'),
    semanticsValue: properties.getString('semantics-value'),
  );
  return Eval.pure(IrNativeValue(Value(circularProgressIndicatorWidget)));
}
