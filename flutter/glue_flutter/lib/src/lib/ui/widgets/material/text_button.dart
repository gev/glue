import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TextButton widget function
/// Creates Flutter TextButton from Glue (text-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir textButton = IrNativeFunc(textButtonImpl);

/// TextButton implementation - takes properties object with keyword arguments
Eval<Ir> textButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTextButton(
    Properties(properties.unlock),
  ),
  _ => _createTextButton(Properties.empty()),
};

/// Create TextButton widget from properties object
Eval<Ir> _createTextButton(Properties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  if (properties.disabled) {
    final buttonWidget = TextButton(onPressed: null, child: Text(label));
    return Eval.pure(IrNativeValue(Value(buttonWidget)));
  }

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onPress(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final buttonWidget = TextButton(
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
