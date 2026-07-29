import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CloseButton widget function
/// Creates Flutter CloseButton from Glue (close-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir closeButton = IrNativeFunc(closeButtonImpl);

/// CloseButton implementation - takes properties object with keyword arguments
Eval<Ir> closeButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCloseButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCloseButton(WidgetProperties.empty()),
};

/// Create CloseButton widget from properties object
Eval<Ir> _createCloseButton(WidgetProperties properties) {
  // Get runtime and create callclose
  return getRuntime().map((runtime) {
    final buttonWidget = CloseButton(
      key: properties.key,
      color: properties.getValue<Color>('color'),
      style: properties.getValue<ButtonStyle>('style'),
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
