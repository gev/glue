import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BackButton widget function
/// Creates Flutter BackButton from Glue (back-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir backButton = IrNativeFunc(backButtonImpl);

/// BackButton implementation - takes properties object with keyword arguments
Eval<Ir> backButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBackButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createBackButton(WidgetProperties.empty()),
};

/// Create BackButton widget from properties object
Eval<Ir> _createBackButton(WidgetProperties properties) {
  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final buttonWidget = BackButton(
      key: properties.key,
      color: properties.getValue<Color>('color'),
      style: properties.getValue<ButtonStyle>('style'),
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
