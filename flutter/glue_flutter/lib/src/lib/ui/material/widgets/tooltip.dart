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
      padding: properties.getValue<EdgeInsetsGeometry>('tooltip-padding'),
      margin: properties.getValue<EdgeInsetsGeometry>('tooltip-margin'),
      verticalOffset: properties.getDouble('tooltip-vertical-offset'),
      preferBelow: properties.getBool('tooltip-prefer-below'),
      excludeFromSemantics: properties.getBool(
        'tooltip-exclude-from-semantics',
      ),
      decoration: properties.getValue<Decoration>('tooltip-decoration'),
      textStyle: properties.getValue<TextStyle>('tooltip-text-style'),
      textAlign: properties.getValue<TextAlign>('tooltip-text-align'),
      waitDuration: properties.getValue<Duration>('tooltip-wait-duration'),
      showDuration: properties.getValue<Duration>('tooltip-show-duration'),
      triggerMode: properties.getValue<TooltipTriggerMode>(
        'tooltip-trigger-mode',
      ),
      enableFeedback: properties.getBool('tooltip-enable-feedback'),
      onTriggered: properties
          .getVoidCallback('tooltip-on-triggered')
          ?.call(runtime),
      richMessage: properties.getValue<InlineSpan>('tooltip-rich-message'),
      child: properties.child,
    );
    return IrNativeValue(Value(tooltipWidget));
  });
}
