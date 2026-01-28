import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoButton widget function
/// Creates Flutter CupertinoButton from Glue (button props) expressions
/// Expects keyword arguments: :child, :on-press, etc.
final Ir cupertinoButton = IrNativeFunc(cupertinoButtonImpl);

/// CupertinoButton implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoButton(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoButton(Properties.empty()),
};

/// Create CupertinoButton widget from properties object
Eval<Ir> _createCupertinoButton(Properties properties) {
  return getRuntime().map((runtime) {
    final callback = properties.onPress(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final buttonWidget = CupertinoButton(
      onPressed: callback,
      onLongPress: longPressCallback,
      disabledColor: properties.cupertinoButtonDisabledColor,
      padding: properties.cupertinoButtonPadding,
      pressedOpacity: properties.cupertinoButtonPressedOpacity,
      borderRadius: properties.cupertinoButtonBorderRadius,
      child: properties.child ?? const Text('Button'),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
