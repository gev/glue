import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// TextButton widget function
/// Creates Flutter TextButton from Glue (text-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir textButton = IrNativeFunc(textButtonImpl);

/// TextButton implementation - takes properties object with keyword arguments
Eval<Ir> textButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTextButton(
    MaterialProperties(properties.unlock),
  ),
  _ => _createTextButton(MaterialProperties.empty()),
};

/// Create TextButton widget from properties object
Eval<Ir> _createTextButton(MaterialProperties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final buttonWidget = TextButton(
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
