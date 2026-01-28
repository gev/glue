import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object with keyword arguments
Eval<Ir> buttonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createButton(Properties(properties.unlock)),
  _ => _createButton(Properties.empty()),
};

/// Create Button widget from properties object
Eval<Ir> _createButton(Properties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  if (properties.disabled) {
    final buttonWidget = ElevatedButton(
      onPressed: null,
      onLongPress: null,
      onHover: null,
      onFocusChange: null,
      style: properties.buttonStyle,
      focusNode: properties.focusNode,
      autofocus: properties.autofocus,
      clipBehavior: properties.buttonClipBehavior,
      child: Text(label),
    );
    return Eval.pure(IrNativeValue(Value(buttonWidget)));
  }

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onPress(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final buttonWidget = ElevatedButton(
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
