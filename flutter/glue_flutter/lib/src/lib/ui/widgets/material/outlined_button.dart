import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// OutlinedButton widget function
/// Creates Flutter OutlinedButton from Glue (outlined-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir outlinedButton = IrNativeFunc(outlinedButtonImpl);

/// OutlinedButton implementation - takes properties object with keyword arguments
Eval<Ir> outlinedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createOutlinedButton(
    Properties(properties.unlock),
  ),
  _ => _createOutlinedButton(Properties.empty()),
};

/// Create OutlinedButton widget from properties object
Eval<Ir> _createOutlinedButton(Properties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final buttonWidget = OutlinedButton(
      onPressed: properties.onPress(runtime),
      onLongPress: properties.onLongPress(runtime),
      onHover: properties.onHover,
      onFocusChange: properties.onFocusChange,
      style: properties.buttonStyle,
      focusNode: properties.focusNode,
      autofocus: properties.autofocus,
      clipBehavior: properties.buttonClipBehavior,
      child: Text(label),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
