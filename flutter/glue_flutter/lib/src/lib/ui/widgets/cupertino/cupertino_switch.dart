import 'package:flutter/cupertino.dart';
import 'package:flutter/gestures.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSwitch widget function
/// Creates Flutter CupertinoSwitch from Glue expressions
/// Expects keyword arguments: :value, :on-changed, etc.
final Ir cupertinoSwitch = IrNativeFunc(cupertinoSwitchImpl);

/// CupertinoSwitch implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSwitchImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSwitch(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoSwitch(Properties.empty()),
};

/// Create CupertinoSwitch widget from properties object
Eval<Ir> _createCupertinoSwitch(Properties properties) {
  return getRuntime().map((runtime) {
    final callback = properties.onSwitchChanged != null
        ? (bool value) => properties.onSwitchChanged!(value)
        : null;
    final switchWidget = CupertinoSwitch(
      value: properties.cupertinoSwitchValue,
      onChanged: callback,
      activeColor: properties.activeColor,
      trackColor: properties.inactiveTrackColor,
      thumbColor: properties.activeThumbColor,
      dragStartBehavior:
          properties.drawerDragStartBehavior ?? DragStartBehavior.start,
      focusNode: properties.dropdownFocusNode,
      autofocus: properties.dropdownAutofocus,
    );
    return IrNativeValue(Value(switchWidget));
  });
}
