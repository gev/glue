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
  return getRuntime().map((runtime) {
    final buttonWidget = ElevatedButton(
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
