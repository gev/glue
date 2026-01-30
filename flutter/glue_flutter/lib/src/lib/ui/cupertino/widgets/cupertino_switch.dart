import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSwitch widget function
/// Creates Flutter CupertinoSwitch from Glue expressions
/// Expects keyword arguments: :value, :on-changed, etc.
final Ir cupertinoSwitch = IrNativeFunc(cupertinoSwitchImpl);

/// CupertinoSwitch implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSwitchImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSwitch(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSwitch(WidgetProperties.empty()),
};

/// Create CupertinoSwitch widget from properties object
Eval<Ir> _createCupertinoSwitch(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    // final callback = properties.onSwitchChanged != null
    //     ? (bool value) => properties.onSwitchChanged!(value)
    //     : null;
    final switchWidget = CupertinoSwitch(
      key: properties.key,
      value: properties.getBool('value') ?? false,
      onChanged: properties.getValue('on-changed'),
      activeColor: properties.getColor('active-color'),
      trackColor: properties.getColor('track-color'),
      thumbColor: properties.getColor('thumb-color'),
      dragStartBehavior: properties.getValue('drag-start-behavior'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
    );
    return IrNativeValue(Value(switchWidget));
  });
}
