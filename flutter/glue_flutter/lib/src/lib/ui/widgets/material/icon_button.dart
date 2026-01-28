import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// IconButton widget function
/// Creates Flutter IconButton from Glue (icon-button props) expressions
final Ir iconButton = IrNativeFunc(iconButtonImpl);

/// IconButton implementation - takes properties object
Eval<Ir> iconButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createIconButton(
    Properties(properties.unlock),
  ),
  _ => _createIconButton(Properties.empty()),
};

/// Create IconButton widget from properties
Eval<Ir> _createIconButton(Properties properties) {
  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onTap(runtime);
    final longPressCallback = properties.onLongPress(runtime);
    final iconButtonWidget = IconButton(
      icon: properties.child ?? const Icon(Icons.add),
      color: properties.color,
      focusColor: properties.focusColor,
      hoverColor: properties.hoverColor,
      highlightColor: properties.highlightColor,
      splashColor: properties.splashColor,
      disabledColor: properties.disabledColor,
      iconSize: properties.iconButtonIconSize,
      visualDensity: properties.iconButtonVisualDensity,
      padding: properties.iconButtonPadding,
      alignment: properties.iconButtonAlignment,
      splashRadius: properties.splashRadius,
      tooltip: properties.tooltip,
      autofocus: properties.autofocus,
      mouseCursor: properties.mouseCursor,
      focusNode: properties.focusNode,
      onPressed: callback,
    );
    return IrNativeValue(Value(iconButtonWidget));
  });
}
