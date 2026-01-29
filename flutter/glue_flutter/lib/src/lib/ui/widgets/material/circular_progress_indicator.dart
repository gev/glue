import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CircularProgressIndicator widget function
/// Creates Flutter CircularProgressIndicator from Glue (circular-progress-indicator props) expressions
final Ir circularProgressIndicator = IrNativeFunc(
  circularProgressIndicatorImpl,
);

/// CircularProgressIndicator implementation - takes properties object
Eval<Ir> circularProgressIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCircularProgressIndicator(
    Properties(properties.unlock),
  ),
  _ => _createCircularProgressIndicator(Properties.empty()),
};

/// Create CircularProgressIndicator widget from properties
Eval<Ir> _createCircularProgressIndicator(Properties properties) {
  final circularProgressIndicatorWidget = CircularProgressIndicator(
    value: properties.circularProgressValue,
    backgroundColor: properties.circularProgressBackgroundColor,
    color: properties.refreshColor,
    strokeWidth: properties.circularProgressStrokeWidth,
    strokeAlign: properties.circularProgressStrokeAlign,
    strokeCap: properties.circularProgressStrokeCap,
    semanticsLabel: properties.circularProgressSemanticsLabel,
    semanticsValue: properties.circularProgressSemanticsValue,
  );
  return Eval.pure(IrNativeValue(Value(circularProgressIndicatorWidget)));
}
