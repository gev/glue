import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Switch widget function
/// Creates Flutter Switch from Glue (switch props) expressions
final Ir switchWidget = IrNativeFunc(switchImpl);

/// Switch implementation - takes properties object
Eval<Ir> switchImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSwitch(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSwitch(WidgetProperties.empty()),
};

/// Create Switch widget from properties
Eval<Ir> _createSwitch(WidgetProperties properties) {
  final switchWidget = Switch(
    key: properties.key,
    value: properties.getBool('value') ?? false,
    onChanged: properties.getValue('on-changed'),
    activeThumbColor: properties.getColor('active-thumb-color'),
    activeTrackColor: properties.getValue('active-track-color'),
    inactiveThumbColor: properties.getColor('inactive-thumb-color'),
    inactiveTrackColor: properties.getValue('inactive-track-color'),
    activeThumbImage: properties.getValue('active-thumb-image'),
    onActiveThumbImageError: properties.getValue('on-active-thumb-image-error'),
    inactiveThumbImage: properties.getValue('inactive-thumb-image'),
    onInactiveThumbImageError: properties.getValue(
      'on-inactive-thumb-image-error',
    ),
    thumbColor: properties.getValue('thumb-color'),
    trackColor: properties.getValue('track-color'),
    trackOutlineColor: properties.getValue('track-outline-color'),
    trackOutlineWidth: properties.getValue('track-outline-width'),
    thumbIcon: properties.getValue('thumb-icon'),
    materialTapTargetSize: properties.getValue('material-tap-target-size'),
    dragStartBehavior:
        properties.getValue('drag-start-behavior') ?? DragStartBehavior.start,
    mouseCursor: properties.getValue('mouse-cursor'),
    focusColor: properties.getColor('focus-color'),
    hoverColor: properties.getColor('hover-color'),
    overlayColor: properties.getValue('overlay-color'),
    splashRadius: properties.getDouble('splash-radius'),
    focusNode: properties.getValue('focus-node'),
    onFocusChange: properties.getValue('on-focus-change'),
    autofocus: properties.getBool('autofocus') ?? false,
    padding: properties.getValue('padding'),
  );
  return Eval.pure(IrNativeValue(Value(switchWidget)));
}
