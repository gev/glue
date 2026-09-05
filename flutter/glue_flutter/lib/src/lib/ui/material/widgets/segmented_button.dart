import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SegmentedButton widget function
/// Creates Flutter SegmentedButton from Glue (segmented-button props) expressions
final Ir segmentedButton = IrNativeFunc(segmentedButtonImpl);

/// SegmentedButton implementation - takes properties object
Eval<Ir> segmentedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSegmentedButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSegmentedButton(WidgetProperties.empty()),
};

/// Create SegmentedButton widget from properties
Eval<Ir> _createSegmentedButton(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final segmentedButtonWidget = SegmentedButton<Ir>(
      key: properties.key,
      selected: properties.getValue<Set<Ir>>('selected') ?? {},
      segments: properties.getValues<ButtonSegment<Ir>>('segments'),
      onSelectionChanged: properties
          .getCallback<Set<Ir>>('on-selection-changed')
          ?.call(runtime),
      emptySelectionAllowed:
          properties.getBool('empty-selection-allowed') ?? false,
      multiSelectionEnabled:
          properties.getBool('multi-selection-enabled') ?? false,
      showSelectedIcon: properties.getBool('show-selected-icon') ?? true,
      style: properties.getValue<ButtonStyle>('style'),
      selectedIcon: properties.getWidget('selected-icon'),
    );
    return IrNativeValue(Value(segmentedButtonWidget));
  });
}
