import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoButton widget function
/// Creates Flutter CupertinoButton from Glue (button props) expressions
/// Expects keyword arguments: :child, :on-press, etc.
final Ir cupertinoButton = IrNativeFunc(cupertinoButtonImpl);

/// CupertinoButton implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoButton(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoButton(CupertinoProperties.empty()),
};

/// Create CupertinoButton widget from properties object
Eval<Ir> _createCupertinoButton(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final buttonWidget = CupertinoButton(
      onPressed: properties.onPress(runtime),
      onLongPress: properties.onLongPress(runtime),
      disabledColor: properties.cupertinoButtonDisabledColor,
      padding: properties.cupertinoButtonPadding,
      pressedOpacity: properties.cupertinoButtonPressedOpacity,
      borderRadius: properties.cupertinoButtonBorderRadius,
      child: properties.child ?? const Text('Button'),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
