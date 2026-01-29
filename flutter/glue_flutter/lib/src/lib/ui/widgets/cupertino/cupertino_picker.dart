import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CupertinoPicker widget function
/// Creates Flutter CupertinoPicker from Glue expressions
/// Expects keyword arguments: :children, :item-extent, :on-selected-item-changed, etc.
final Ir cupertinoPicker = IrNativeFunc(cupertinoPickerImpl);

/// CupertinoPicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoPickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoPicker(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoPicker(Properties.empty()),
};

/// Create CupertinoPicker widget from properties object
Eval<Ir> _createCupertinoPicker(Properties properties) {
  return getRuntime().map((runtime) {
    final callback = properties.cupertinoPickerOnSelectedItemChanged != null
        ? (int value) => properties.cupertinoPickerOnSelectedItemChanged!(value)
        : null;
    final pickerWidget = CupertinoPicker(
      key: GlobalKey(),
      diameterRatio: properties.cupertinoPickerDiameterRatio,
      backgroundColor: properties.cupertinoPickerBackgroundColor,
      offAxisFraction: properties.cupertinoPickerOffAxisFraction,
      useMagnifier: properties.cupertinoPickerUseMagnifier,
      magnification: properties.cupertinoPickerMagnification,
      scrollController: properties.cupertinoPickerScrollController,
      squeeze: properties.cupertinoPickerSqueeze,
      itemExtent: properties.cupertinoPickerItemExtent,
      onSelectedItemChanged: callback,
      children: properties.children,
      selectionOverlay:
          properties.cupertinoPickerSelectionOverlay ??
          const CupertinoPickerDefaultSelectionOverlay(),
    );
    return IrNativeValue(Value(pickerWidget));
  });
}
