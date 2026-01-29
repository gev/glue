import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// SegmentedButton widget function
/// Creates Flutter SegmentedButton from Glue (segmented-button props) expressions
final Ir segmentedButton = IrNativeFunc(segmentedButtonImpl);

/// SegmentedButton implementation - takes properties object
Eval<Ir> segmentedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSegmentedButton(
    Properties(properties.unlock),
  ),
  _ => _createSegmentedButton(Properties.empty()),
};

/// Create SegmentedButton widget from properties
Eval<Ir> _createSegmentedButton(Properties properties) {
  // Convert widgets to ButtonSegment objects
  final segments = (properties.segmentedSegments ?? []).map((widget) {
    return ButtonSegment(
      value: widget, // Use the widget itself as the value
      label: widget,
    );
  }).toList();

  final segmentedButtonWidget = SegmentedButton(
    selected: properties.segmentedSelected ?? <dynamic>{},
    segments: segments,
    onSelectionChanged: properties.onSegmentedSelectionChanged,
    multiSelectionEnabled: properties.multiSelectionEnabledFor != null,
    showSelectedIcon: properties.showSelectedIcon ?? true,
    style: ButtonStyle(
      backgroundColor: WidgetStateProperty.all(
        properties.segmentedBackgroundColor,
      ),
      foregroundColor: WidgetStateProperty.all(
        properties.segmentedUnselectedColor,
      ),
      overlayColor: WidgetStateProperty.all(properties.segmentedSelectedColor),
      shadowColor: WidgetStateProperty.all(properties.segmentedShadowColor),
      surfaceTintColor: WidgetStateProperty.all(
        properties.segmentedSurfaceTintColor,
      ),
      elevation: WidgetStateProperty.all(properties.segmentedElevation),
    ),
  );
  return Eval.pure(IrNativeValue(Value(segmentedButtonWidget)));
}
