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
    value: properties.progressValue,
    backgroundColor: properties.color, // using color for background
    color: properties.activeColor, // using activeColor for progress color
    valueColor: properties.valueColor,
    minHeight: properties.progressMinHeight,
    semanticsLabel: properties.progressSemanticsLabel,
    semanticsValue: properties.progressSemanticsValue,
  );
  return Eval.pure(IrNativeValue(Value(progressIndicatorWidget)));
}
