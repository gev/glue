import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// IconButton widget function
/// Creates Flutter IconButton from Glue (icon-button props) expressions
final Ir iconButton = IrNativeFunc(iconButtonImpl);

/// IconButton implementation - takes properties object
Eval<Ir> iconButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createIconButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createIconButton(WidgetProperties.empty()),
};

/// Create IconButton widget from properties
Eval<Ir> _createIconButton(WidgetProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final iconButtonWidget = IconButton(
      key: properties.key,
      icon: properties.child ?? const Icon(Icons.add),
      color: properties.getColor('color'),
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      highlightColor: properties.getColor('highlight-color'),
      splashColor: properties.getColor('splash-color'),
      disabledColor: properties.getColor('disabled-color'),
      iconSize: properties.getDouble('icon-size'),
      visualDensity: properties.getValue<VisualDensity>('visual-density'),
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
      alignment: properties.getValue<AlignmentGeometry>('alignment'),
      splashRadius: properties.getDouble('splash-radius'),
      tooltip: properties.getString('tooltip'),
      autofocus: properties.getBool('autofocus') ?? false,
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
      onLongPress: properties.getVoidCallback('on-long-press')?.call(runtime),
    );
    return IrNativeValue(Value(iconButtonWidget));
  });
}
