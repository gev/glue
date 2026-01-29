import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object with keyword arguments
Eval<Ir> buttonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createButton(WidgetProperties.empty()),
};

/// Create Button widget from properties object
Eval<Ir> _createButton(WidgetProperties properties) {
  final label =
      properties.getString('label') ??
      'Button'; // Extract label from properties

  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final buttonWidget = ElevatedButton(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onHover: properties.getValue('on-hover'),
      onFocusChange: properties.getValue('on-focus-change'),
      style: properties.getValue('style'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue('clip-behavior'),
      child: Text(label),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
