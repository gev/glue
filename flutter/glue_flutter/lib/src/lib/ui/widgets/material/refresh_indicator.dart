import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// RefreshIndicator widget function
/// Creates Flutter RefreshIndicator from Glue (refresh-indicator props) expressions
final Ir refreshIndicator = IrNativeFunc(refreshIndicatorImpl);

/// RefreshIndicator implementation - takes properties object
Eval<Ir> refreshIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRefreshIndicator(
    Properties(properties.unlock),
  ),
  _ => _createRefreshIndicator(Properties.empty()),
};

/// Create RefreshIndicator widget from properties
Eval<Ir> _createRefreshIndicator(Properties properties) {
  final refreshIndicatorWidget = RefreshIndicator(
    child: properties.child ?? const SizedBox(),
    displacement: properties.refreshDisplacement,
    edgeOffset: properties.refreshEdgeOffset,
    onRefresh: properties.materialRefreshOnRefresh ?? () async {},
    color: properties.refreshColor,
    backgroundColor: properties.refreshBackgroundColor,
    notificationPredicate: properties.refreshNotificationPredicate,
    semanticsLabel: properties.refreshSemanticsLabel,
    semanticsValue: properties.refreshSemanticsValue,
    strokeWidth: properties.refreshStrokeWidth,
    triggerMode: properties.refreshTriggerMode,
  );
  return Eval.pure(IrNativeValue(Value(refreshIndicatorWidget)));
}
