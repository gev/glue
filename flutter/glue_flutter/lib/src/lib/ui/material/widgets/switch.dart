import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  return getRuntime().map((runtime) {
    final switchWidget = Switch(
      key: properties.key,
      value: properties.getBool('value') ?? false,
      onChanged: properties.getCallback<bool>('on-changed')?.call(runtime),
      activeThumbColor: properties.getColor('active-thumb-color'),
      activeTrackColor: properties.getColor('active-track-color'),
      inactiveThumbColor: properties.getColor('inactive-thumb-color'),
      inactiveTrackColor: properties.getColor('inactive-track-color'),
      activeThumbImage: properties.getValue<ImageProvider>(
        'active-thumb-image',
      ),
      inactiveThumbImage: properties.getValue<ImageProvider>(
        'inactive-thumb-image',
      ),
      thumbColor: properties.getValue<WidgetStateProperty<Color?>>(
        'thumb-color',
      ),
      trackColor: properties.getValue<WidgetStateProperty<Color?>>(
        'track-color',
      ),
      trackOutlineColor: properties.getValue<WidgetStateProperty<Color?>>(
        'track-outline-color',
      ),
      trackOutlineWidth: properties.getValue<WidgetStateProperty<double?>>(
        'track-outline-width',
      ),
      thumbIcon: properties.getValue<WidgetStateProperty<Icon?>>('thumb-icon'),
      materialTapTargetSize: properties.getValue<MaterialTapTargetSize>(
        'material-tap-target-size',
      ),
      dragStartBehavior:
          properties.getValue<DragStartBehavior>('drag-start-behavior') ??
          DragStartBehavior.start,
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      overlayColor: properties.getValue<WidgetStateProperty<Color?>>(
        'overlay-color',
      ),
      splashRadius: properties.getDouble('splash-radius'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      onFocusChange: properties
          .getCallback<bool>('on-focus-change')
          ?.call(runtime),
      autofocus: properties.getBool('autofocus') ?? false,
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    );
    return IrNativeValue(Value(switchWidget));
  });
}
