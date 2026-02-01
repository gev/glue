import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// RefreshIndicator widget function
/// Creates Flutter RefreshIndicator from Glue (refresh-indicator props) expressions
final Ir refreshIndicator = IrNativeFunc(refreshIndicatorImpl);

/// RefreshIndicator implementation - takes properties object
Eval<Ir> refreshIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRefreshIndicator(
    WidgetProperties(properties.unlock),
  ),
  _ => _createRefreshIndicator(WidgetProperties.empty()),
};

/// Create RefreshIndicator widget from properties
Eval<Ir> _createRefreshIndicator(WidgetProperties properties) {
  final onRefresh = properties.getValue<RefreshCallback>('on-refresh');
  if (onRefresh == null) {
    return throwError(wrongArgumentType(['on-refresh']));
  }
  return getRuntime().map((runtime) {
    final refreshIndicatorWidget = RefreshIndicator(
      key: properties.key,
      child: properties.child ?? const SizedBox(),
      displacement: properties.getDouble('displacement') ?? 40.0,
      edgeOffset: properties.getDouble('edge-offset') ?? 0.0,
      onRefresh: onRefresh,
      color: properties.getColor('color'),
      backgroundColor: properties.getColor('background-color'),
      semanticsLabel: properties.getString('semantics-label'),
      semanticsValue: properties.getString('semantics-value'),
      strokeWidth:
          properties.getDouble('stroke-width') ??
          RefreshProgressIndicator.defaultStrokeWidth,
      triggerMode:
          properties.getValue<RefreshIndicatorTriggerMode>('trigger-mode') ??
          RefreshIndicatorTriggerMode.onEdge,
    );
    return IrNativeValue(Value(refreshIndicatorWidget));
  });
}
