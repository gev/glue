import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TextButton widget function
/// Creates Flutter TextButton from Glue (text-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir textButton = IrNativeFunc(textButtonImpl);

/// TextButton implementation - takes properties object with keyword arguments
Eval<Ir> textButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTextButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTextButton(WidgetProperties.empty()),
};

/// Create TextButton widget from properties object
Eval<Ir> _createTextButton(WidgetProperties properties) {
  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final buttonWidget = TextButton(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onHover: properties.getValue('on-hover'),
      onFocusChange: properties.getValue('on-focus-change'),
      style: properties.getValue('style'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue('clip-behavior'),
      child: properties.child ?? Text('Button'),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
