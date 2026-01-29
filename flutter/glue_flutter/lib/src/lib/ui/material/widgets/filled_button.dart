import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FilledButton widget function
/// Creates Flutter FilledButton from Glue (filled-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir filledButton = IrNativeFunc(filledButtonImpl);

/// FilledButton implementation - takes properties object with keyword arguments
Eval<Ir> filledButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFilledButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createFilledButton(WidgetProperties.empty()),
};

/// Create FilledButton widget from properties object
Eval<Ir> _createFilledButton(WidgetProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final buttonWidget = FilledButton(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onHover: properties.getValue('on-hover'),
      onFocusChange: properties.getValue('on-focus-change'),
      style: properties.getValue('button-style'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.none,
      child: properties.child ?? const Text('Button'),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
