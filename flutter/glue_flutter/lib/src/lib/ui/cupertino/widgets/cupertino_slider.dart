import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSlider widget function
/// Creates Flutter CupertinoSlider from Glue expressions
/// Expects keyword arguments: :value, :on-changed, :min, :max, etc.
final Ir cupertinoSlider = IrNativeFunc(cupertinoSliderImpl);

/// CupertinoSlider implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSliderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSlider(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSlider(WidgetProperties.empty()),
};

/// Create CupertinoSlider widget from properties object
Eval<Ir> _createCupertinoSlider(WidgetProperties properties) {
  final widget = CupertinoSlider(
    key: properties.key,
    value: properties.getDouble('value') ?? 0.5,
    onChanged: properties.getValue<>('on-changed'),
    onChangeStart: properties.getValue<>('on-change-start'),
    onChangeEnd: properties.getValue<>('on-change-end'),
    min: properties.getDouble('min') ?? 0.0,
    max: properties.getDouble('max') ?? 1.0,
    divisions: properties.getInt('divisions'),
    activeColor: properties.getColor('active-color'),
    thumbColor: properties.getColor('thumb-color') ?? const Color(0xFFFFFFFF),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
