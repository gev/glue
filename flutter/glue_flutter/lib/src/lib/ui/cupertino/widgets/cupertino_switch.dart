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
      value: properties.getBool('value') ?? false,
      onChanged: properties.getValue('on-changed'),
      thumbColor: properties.getColor('thumb-color'),
      inactiveThumbColor: properties.getColor('inactive-thumb-color'),
      activeTrackColor: properties.getColor('active-track-color'),
      inactiveTrackColor: properties.getColor('inactive-track-color'),
      activeThumbImage: properties.getValue('active-thumb-image'),
      inactiveThumbImage: properties.getValue('inactive-thumb-image'),
      onActiveThumbImageError: properties.getValue(
        'on-active-thumb-image-error',
      ),
      onInactiveThumbImageError: properties.getValue(
        'on-inactive-thumb-image-error',
      ),
      dragStartBehavior: properties.getValue('drag-start-behavior'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getValue('autofocus'),
    );
    return IrNativeValue(Value(switchWidget));
  });
}
