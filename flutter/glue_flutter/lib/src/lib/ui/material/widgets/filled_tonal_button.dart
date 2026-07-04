import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FilledTonalButton widget function
/// Creates Flutter FilledTonalButton from Glue (filled-tonal-button props) expressions
/// Expects keyword arguments: :label, :on-press, :disabled, etc.
final Ir filledTonalButton = IrNativeFunc(filledTonalButtonImpl);

/// FilledTonalButton implementation - takes properties object with keyword arguments
Eval<Ir> filledTonalButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFilledTonalButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createFilledTonalButton(WidgetProperties.empty()),
};

/// Create FilledTonalButton widget from properties object
Eval<Ir> _createFilledTonalButton(WidgetProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final buttonWidget = FilledButton.tonal(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
      onLongPress: properties.getVoidCallback('on-long-press')?.call(runtime),
      onHover: properties.getCallback<bool>('on-hover')?.call(runtime),
      onFocusChange: properties
          .getCallback<bool>('on-focus-change')
          ?.call(runtime),
      style: properties.getValue<ButtonStyle>('style'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue<Clip>('clip-behavior'),
      child: properties.child,
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
