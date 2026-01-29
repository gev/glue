import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FloatingActionButton widget function
/// Creates Flutter FloatingActionButton from Glue (floating-action-button props) expressions
final Ir floatingActionButton = IrNativeFunc(floatingActionButtonImpl);

/// FloatingActionButton implementation - takes properties object
Eval<Ir> floatingActionButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFloatingActionButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createFloatingActionButton(WidgetProperties.empty()),
};

/// Create FloatingActionButton widget from properties
Eval<Ir> _createFloatingActionButton(WidgetProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final fabWidget = FloatingActionButton(
      key: properties.key,
      child: properties.child,
      tooltip: properties.getString('tooltip'),
      foregroundColor: properties.getColor('foreground-color'),
      backgroundColor: properties.getColor('color'),
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      splashColor: properties.getColor('splash-color'),
      heroTag: properties.getValue('hero-tag') ?? const Object(),
      elevation: properties.getDouble('elevation'),
      focusElevation: properties.getDouble('focus-elevation'),
      hoverElevation: properties.getDouble('hover-elevation'),
      highlightElevation: properties.getDouble('highlight-elevation'),
      disabledElevation: properties.getDouble('disabled-elevation'),
      mini: properties.getBool('mini') ?? false,
      shape: properties.getValue('shape'),
      clipBehavior: properties.getValue('clip-behavior'),
      isExtended: properties.getBool('is-extended') ?? false,
      autofocus: properties.getBool('autofocus') ?? false,
      materialTapTargetSize: properties.getValue('material-tap-target-size'),
      enableFeedback: properties.getBool('enable-feedback') ?? true,
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      mouseCursor: properties.getValue('mouse-cursor'),
      focusNode: properties.getValue('focus-node'),
    );
    return IrNativeValue(Value(fabWidget));
  });
}
