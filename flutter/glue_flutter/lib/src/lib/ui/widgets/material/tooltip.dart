import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// Tooltip widget function
/// Creates Flutter Tooltip from Glue (tooltip props) expressions
final Ir tooltip = IrNativeFunc(tooltipImpl);

/// Tooltip implementation - takes properties object
Eval<Ir> tooltipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTooltip(Properties(properties.unlock)),
  _ => _createTooltip(Properties.empty()),
};

/// Create Tooltip widget from properties
Eval<Ir> _createTooltip(Properties properties) {
  final tooltipWidget = Tooltip(
    message: properties.tooltipMessage,
    height: properties.tooltipHeight,
    padding: properties.tooltipPadding,
    margin: properties.tooltipMargin,
    verticalOffset: properties.tooltipVerticalOffset,
    preferBelow: properties.tooltipPreferBelow,
    excludeFromSemantics: properties.tooltipExcludeFromSemantics,
    decoration: properties.tooltipDecoration,
    textStyle: properties.tooltipTextStyle,
    textAlign: properties.tooltipTextAlign,
    waitDuration: properties.tooltipWaitDuration,
    showDuration: properties.tooltipShowDuration,
    triggerMode: properties.tooltipTriggerMode,
    enableFeedback: properties.tooltipEnableFeedback,
    onTriggered: properties.tooltipOnTriggered,
    richMessage: properties.tooltipRichMessage,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(tooltipWidget)));
}
