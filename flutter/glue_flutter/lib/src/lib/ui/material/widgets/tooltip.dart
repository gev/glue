import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Tooltip widget function
/// Creates Flutter Tooltip from Glue (tooltip props) expressions
final Ir tooltip = IrNativeFunc(tooltipImpl);

/// Tooltip implementation - takes properties object
Eval<Ir> tooltipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTooltip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTooltip(WidgetProperties.empty()),
};

/// Create Tooltip widget from properties
Eval<Ir> _createTooltip(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final tooltipWidget = Tooltip(
      key: properties.key,
      message: properties.getString('tooltip-message'),
      padding: properties.getValue('tooltip-padding'),
      margin: properties.getValue('tooltip-margin'),
      verticalOffset: properties.getDouble('tooltip-vertical-offset'),
      preferBelow: properties.getBool('tooltip-prefer-below'),
      excludeFromSemantics: properties.getBool(
        'tooltip-exclude-from-semantics',
      ),
      decoration: properties.getValue('tooltip-decoration'),
      textStyle: properties.getValue('tooltip-text-style'),
      textAlign: properties.getValue('tooltip-text-align'),
      waitDuration: properties.getValue('tooltip-wait-duration'),
      showDuration: properties.getValue('tooltip-show-duration'),
      triggerMode: properties.getValue('tooltip-trigger-mode'),
      enableFeedback: properties.getBool('tooltip-enable-feedback'),
      onTriggered: properties.getVoidCallback('tooltip-on-triggered', runtime),
      richMessage: properties.getValue('tooltip-rich-message'),
      child: properties.child,
    );
    return IrNativeValue(Value(tooltipWidget));
  });
}
