import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// OutlinedButton widget function
/// Creates Flutter OutlinedButton from Glue (outlined-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir outlinedButton = IrNativeFunc(outlinedButtonImpl);

/// OutlinedButton implementation - takes properties object with keyword arguments
Eval<Ir> outlinedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createOutlinedButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createOutlinedButton(WidgetProperties.empty()),
};

/// Create OutlinedButton widget from properties object
Eval<Ir> _createOutlinedButton(WidgetProperties properties) {
  final label =
      properties.getString('label') ??
      'Button'; // Extract label from properties

  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final buttonWidget = OutlinedButton(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-press', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onHover: properties.getValue('on-hover'),
      onFocusChange: properties.getValue('on-focus-change'),
      style: properties.getValue('button-style'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue('button-clip-behavior'),
      child: Text(label),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
