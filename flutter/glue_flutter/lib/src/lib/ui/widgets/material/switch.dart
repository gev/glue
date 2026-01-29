import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// Switch widget function
/// Creates Flutter Switch from Glue (switch props) expressions
final Ir switchWidget = IrNativeFunc(switchImpl);

/// Switch implementation - takes properties object
Eval<Ir> switchImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSwitch(Properties(properties.unlock)),
  _ => _createSwitch(Properties.empty()),
};

/// Create Switch widget from properties
Eval<Ir> _createSwitch(Properties properties) {
  final switchWidget = Switch(
    value: properties.switchValue,
    onChanged: properties.onSwitchChanged,
    activeColor: properties.activeColor,
    activeThumbColor: properties.activeThumbColor,
    activeTrackColor: properties.activeTrackColor,
    inactiveThumbColor: properties.inactiveThumbColor,
    inactiveTrackColor: properties.inactiveTrackColor,
    activeThumbImage: properties.activeThumbImage,
    onActiveThumbImageError: properties.onActiveThumbImageError,
    inactiveThumbImage: properties.inactiveThumbImage,
    onInactiveThumbImageError: properties.onInactiveThumbImageError,
    thumbColor: properties.thumbColor,
    trackColor: properties.trackColor,
    trackOutlineColor: properties.trackOutlineColor,
    trackOutlineWidth: properties.trackOutlineWidth,
    thumbIcon: properties.thumbIcon,
    materialTapTargetSize: properties.materialTapTargetSize,
    dragStartBehavior:
        properties.drawerDragStartBehavior ?? DragStartBehavior.start,
    mouseCursor: properties.mouseCursor,
    focusColor: properties.focusColor,
    hoverColor: properties.hoverColor,
    overlayColor: properties.overlayColor,
    splashRadius: properties.splashRadius,
    focusNode: properties.focusNode,
    onFocusChange: properties.onFocusChange,
    autofocus: properties.autofocus,
    padding: properties.switchPadding,
  );
  return Eval.pure(IrNativeValue(Value(switchWidget)));
}
