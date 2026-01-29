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
      key: GlobalKey(),
      diameterRatio: properties.getValue('diameter-ratio'),
      backgroundColor: properties.getValue('background-color'),
      offAxisFraction: properties.getValue('off-axis-fraction'),
      useMagnifier: properties.getValue('use-magnifier'),
      magnification: properties.getValue('magnification'),
      scrollController: properties.getValue('scroll-controller'),
      squeeze: properties.getValue('squeeze'),
      itemExtent: properties.getValue('item-extent'),
      onSelectedItemChanged: properties.getValue('on-selected-item-changed'),
      children: properties.children,
      selectionOverlay:
          properties.getValue('selection-overlay') ??
          const CupertinoPickerDefaultSelectionOverlay(),
    );
    return IrNativeValue(Value(pickerWidget));
  });
}
