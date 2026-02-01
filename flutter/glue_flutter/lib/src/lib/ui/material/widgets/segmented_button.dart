import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final selected = properties.getValue<Set<dynamic>>('selected');
  if (selected == null) {
    return throwError(wrongArgumentType(['selected']));
  }
  return getRuntime().map((runtime) {
    final segmentedButtonWidget = SegmentedButton<dynamic>(
      key: properties.key,
      selected: selected,
      segments: properties.getValues('segments'),
      onSelectionChanged: properties
          .getCallback<Set<dynamic>>('on-selection-changed')
          ?.call(runtime),
      showSelectedIcon: properties.getBool('show-selected-icon') ?? true,
    );
    return IrNativeValue(Value(segmentedButtonWidget));
  });
}
