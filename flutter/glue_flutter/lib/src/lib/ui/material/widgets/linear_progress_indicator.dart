import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// LinearProgressIndicator widget function
/// Creates Flutter LinearProgressIndicator from Glue (linear-progress-indicator props) expressions
final Ir linearProgressIndicator = IrNativeFunc(linearProgressIndicatorImpl);

/// LinearProgressIndicator implementation - takes properties object
Eval<Ir> linearProgressIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createLinearProgressIndicator(
    WidgetProperties(properties.unlock),
  ),
  _ => _createLinearProgressIndicator(WidgetProperties.empty()),
};

/// Create LinearProgressIndicator widget from properties
Eval<Ir> _createLinearProgressIndicator(WidgetProperties properties) {
  final progressIndicatorWidget = LinearProgressIndicator(
    key: properties.key,
    value: properties.getDouble('value'),
    backgroundColor: properties.getColor('background-color'),
    color: properties.getColor('color'),
    valueColor: properties.getValue<>('value-color'),
    minHeight: properties.getDouble('min-height'),
    semanticsLabel: properties.getString('semantics-label'),
    semanticsValue: properties.getString('semantics-value'),
  );
  return Eval.pure(IrNativeValue(Value(progressIndicatorWidget)));
}
