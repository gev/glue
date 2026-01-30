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
    final widget = CupertinoButton(
      key: properties.key,
      child: properties.child ?? const Text('Button'),
      padding: properties.getValue('padding'),
      color: properties.getColor('color'),
      disabledColor:
          properties.getColor('disabled-color') ??
          CupertinoColors.quaternarySystemFill,
      minSize: properties.getDouble('min-size'),
      pressedOpacity: properties.getDouble('pressed-opacity'),
      borderRadius: properties.getValue('border-radius'),
      alignment: properties.getValue('alignment'),
      onPressed: properties.getVoidCallback('on-pressed', runtime),
    );
    return IrNativeValue(Value(widget));
  });
}
