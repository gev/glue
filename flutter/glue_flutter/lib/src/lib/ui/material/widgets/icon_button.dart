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
      onPressed: properties.onPress(runtime),
      onLongPress: properties.onLongPress(runtime),
    );
    return IrNativeValue(Value(iconButtonWidget));
  });
}
