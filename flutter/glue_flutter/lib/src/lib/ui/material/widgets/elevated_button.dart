import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ElevatedButton widget function
/// Creates Flutter ElevatedButton from Glue (elevated-button props) expressions
final Ir elevatedButton = IrNativeFunc(elevatedButtonImpl);

/// ElevatedButton implementation - takes properties object
Eval<Ir> elevatedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createElevatedButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createElevatedButton(WidgetProperties.empty()),
};

/// Create ElevatedButton widget from properties
Eval<Ir> _createElevatedButton(WidgetProperties properties) {
  final label =
      properties.getString('label') ??
      'Button'; // Extract label from properties

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final buttonWidget = ElevatedButton(
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
