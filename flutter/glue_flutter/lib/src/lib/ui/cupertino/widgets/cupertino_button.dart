import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoButton widget function
/// Creates Flutter CupertinoButton from Glue (button props) expressions
/// Expects keyword arguments: :child, :on-press, etc.
final Ir cupertinoButton = IrNativeFunc(cupertinoButtonImpl);

/// CupertinoButton implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoButton(WidgetProperties.empty()),
};

/// Create CupertinoButton widget from properties object
Eval<Ir> _createCupertinoButton(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final buttonWidget = CupertinoButton(
      onPressed: properties.getVoidCallback('on-press', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      disabledColor: properties.getValue('cupertino-button-disabled-color'),
      padding: properties.getValue('cupertino-button-padding'),
      pressedOpacity: properties.getDouble('cupertino-button-pressed-opacity'),
      borderRadius: properties.getValue('cupertino-button-border-radius'),
      child: properties.child ?? const Text('Button'),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
