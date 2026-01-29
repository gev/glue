import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoTimerPicker widget function
/// Creates Flutter CupertinoTimerPicker from Glue expressions
/// Expects keyword arguments: :mode, :initial-timer-duration, :minute-interval, :second-interval, :alignment, :background-color, :on-timer-duration-changed
final Ir cupertinoTimerPicker = IrNativeFunc(cupertinoTimerPickerImpl);

/// CupertinoTimerPicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTimerPickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTimerPicker(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoTimerPicker(CupertinoProperties.empty()),
};

/// Create CupertinoTimerPicker widget from properties object
Eval<Ir> _createCupertinoTimerPicker(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoTimerPicker(
      mode: properties.cupertinoTimerPickerMode,
      initialTimerDuration: properties.cupertinoTimerPickerInitialTimerDuration,
      minuteInterval: properties.cupertinoTimerPickerMinuteInterval,
      secondInterval: properties.cupertinoTimerPickerSecondInterval,
      alignment: properties.cupertinoTimerPickerAlignment,
      backgroundColor: properties.cupertinoTimerPickerBackgroundColor,
      itemExtent: properties.cupertinoTimerPickerItemExtent,
      onTimerDurationChanged:
          properties.cupertinoTimerPickerOnTimerDurationChanged,
    );
    return IrNativeValue(Value(widget));
  });
}
