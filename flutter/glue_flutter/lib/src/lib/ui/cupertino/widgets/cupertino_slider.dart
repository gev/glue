import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoSlider widget function
/// Creates Flutter CupertinoSlider from Glue expressions
/// Expects keyword arguments: :value, :on-changed, :min, :max, etc.
final Ir cupertinoSlider = IrNativeFunc(cupertinoSliderImpl);

/// CupertinoSlider implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSliderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSlider(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoSlider(CupertinoProperties.empty()),
};

/// Create CupertinoSlider widget from properties object
Eval<Ir> _createCupertinoSlider(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoSlider(
      value: properties.cupertinoSliderValue,
      onChanged: properties.cupertinoSliderOnChanged,
      onChangeStart: properties.cupertinoSliderOnChangeStart,
      onChangeEnd: properties.cupertinoSliderOnChangeEnd,
      min: properties.cupertinoSliderMin,
      max: properties.cupertinoSliderMax,
      divisions: properties.cupertinoSliderDivisions,
      activeColor: properties.cupertinoSliderActiveColor,
      thumbColor: properties.cupertinoSliderThumbColor,
    );
    return IrNativeValue(Value(widget));
  });
}
