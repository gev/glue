import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTimerPicker widget function
/// Creates Flutter CupertinoTimerPicker from Glue expressions
/// Expects keyword arguments: :mode, :initial-timer-duration, :minute-interval, :second-interval, :alignment, :background-color, :on-timer-duration-changed
final Ir cupertinoTimerPicker = IrNativeFunc(cupertinoTimerPickerImpl);

/// CupertinoTimerPicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTimerPickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTimerPicker(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoTimerPicker(WidgetProperties.empty()),
};

/// Create CupertinoTimerPicker widget from properties object
Eval<Ir> _createCupertinoTimerPicker(WidgetProperties properties) {
  final widget = CupertinoTimerPicker(
    mode: properties.getValue('mode'),
    initialTimerDuration: properties.getValue('initial-timer-duration'),
    minuteInterval: properties.getInt('minute-interval') ?? 1,
    secondInterval: properties.getInt('second-interval') ?? 1,
    alignment: properties.getValue('alignment') ?? Alignment.center,
    backgroundColor: properties.getColor('background-color'),
    itemExtent: properties.getDouble('item-extent') ?? 32.0,
    onTimerDurationChanged: properties.getValue('on-timer-duration-changed'),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
