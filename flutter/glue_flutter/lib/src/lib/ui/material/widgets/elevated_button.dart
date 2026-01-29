import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// ElevatedButton widget function
/// Creates Flutter ElevatedButton from Glue (elevated-button props) expressions
final Ir elevatedButton = IrNativeFunc(elevatedButtonImpl);

/// ElevatedButton implementation - takes properties object
Eval<Ir> elevatedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createElevatedButton(
    MaterialProperties(properties.unlock),
  ),
  _ => _createElevatedButton(MaterialProperties.empty()),
};

/// Create ElevatedButton widget from properties
Eval<Ir> _createElevatedButton(MaterialProperties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final buttonWidget = ElevatedButton(
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
