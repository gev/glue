import 'package:flutter/cupertino.dart';
import 'package:glue/error.dart';
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
  final onChanged = properties.getCallback<double>('on-changed');
  if (onChanged == null) {
    return throwError(wrongArgumentType(['on-changed required']));
  }
  return getRuntime().map((runtime) {
    final widget = CupertinoSlider(
      key: properties.key,
      value: properties.getDouble('value') ?? 0.5,
      onChanged: onChanged(runtime),
      onChangeStart: properties
          .getCallback<double>('on-change-start')
          ?.call(runtime),
      onChangeEnd: properties
          .getCallback<double>('on-change-end')
          ?.call(runtime),
      min: properties.getDouble('min') ?? 0.0,
      max: properties.getDouble('max') ?? 1.0,
      divisions: properties.getInt('divisions'),
      activeColor: properties.getColor('active-color'),
      thumbColor: properties.getColor('thumb-color') ?? const Color(0xFFFFFFFF),
    );
    return IrNativeValue(Value(widget));
  });
}
