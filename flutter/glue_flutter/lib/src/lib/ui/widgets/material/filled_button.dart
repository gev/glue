import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FilledButton widget function
/// Creates Flutter FilledButton from Glue (filled-button props) expressions
/// Expects keyword arguments: :label, :on-tap, :disabled, etc.
final Ir filledButton = IrNativeFunc(filledButtonImpl);

/// FilledButton implementation - takes properties object with keyword arguments
Eval<Ir> filledButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFilledButton(
    Properties(properties.unlock),
  ),
  _ => _createFilledButton(Properties.empty()),
};

/// Create FilledButton widget from properties object
Eval<Ir> _createFilledButton(Properties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  if (properties.disabled) {
    final buttonWidget = FilledButton(onPressed: null, child: Text(label));
    return Eval.pure(IrNativeValue(Value(buttonWidget)));
  }

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onTap(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final buttonWidget = FilledButton(
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
