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
    final segmentedButtonWidget = SegmentedButton<dynamic>(
      key: properties.key,
      selected: properties.getValue('selected'),
      segments: properties.getWidgets('segments'),
      onSelectionChanged: properties.getValue('on-selection-changed'),
      multiSelectionEnabledFor: properties.getValue(
        'multi-selection-enabled-for',
      ),
      showSelectedIcon: properties.getBool('show-selected-icon'),
      backgroundColor: properties.getColor('background-color'),
      unselectedColor: properties.getColor('unselected-color'),
      selectedColor: properties.getColor('selected-color'),
      disabledColor: properties.getColor('disabled-color'),
      shadowColor: properties.getColor('shadow-color'),
      surfaceTintColor: properties.getColor('surface-tint-color'),
      elevation: properties.getDouble('elevation'),
      padding: properties.getValue('padding'),
      visualDensity: properties.getValue('visual-density'),
      materialTapTargetSize: properties.getValue('material-tap-target-size'),
    );
    return IrNativeValue(Value(segmentedButtonWidget));
  });
}
