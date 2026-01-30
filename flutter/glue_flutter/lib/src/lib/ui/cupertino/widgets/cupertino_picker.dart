import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoPicker widget function
/// Creates Flutter CupertinoPicker from Glue expressions
/// Expects keyword arguments: :children, :item-extent, :on-selected-item-changed, etc.
final Ir cupertinoPicker = IrNativeFunc(cupertinoPickerImpl);

/// CupertinoPicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoPickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoPicker(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoPicker(WidgetProperties.empty()),
};

/// Create CupertinoPicker widget from properties object
Eval<Ir> _createCupertinoPicker(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final pickerWidget = CupertinoPicker(
      key: properties.key,
      diameterRatio: properties.getDouble('diameter-ratio') ?? 1.07,
      backgroundColor: properties.getColor('background-color'),
      offAxisFraction: properties.getDouble('off-axis-fraction') ?? 0.0,
      useMagnifier: properties.getBool('use-magnifier') ?? false,
      magnification: properties.getDouble('magnification') ?? 1.0,
      scrollController: properties.getValue('scroll-controller'),
      squeeze: properties.getDouble('squeeze') ?? 1.25,
      itemExtent: properties.getDouble('item-extent') ?? 44.0,
      onSelectedItemChanged: properties.getValue('on-selected-item-changed'),
      children: properties.children,
      selectionOverlay:
          properties.getValue('selection-overlay') ??
          const CupertinoPickerDefaultSelectionOverlay(),
    );
    return IrNativeValue(Value(pickerWidget));
  });
}
