import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// OutlinedButton widget function
/// Creates Flutter OutlinedButton from Glue (outlined-button props) expressions
/// Expects keyword arguments: :label, :on-tap, :disabled, etc.
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

  if (properties.disabled) {
    final buttonWidget = OutlinedButton(onPressed: null, child: Text(label));
    return Eval.pure(IrNativeValue(Value(buttonWidget)));
  }

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onTap(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final buttonWidget = OutlinedButton(
      onPressed: callback,
      onLongPress: longPressCallback,
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
