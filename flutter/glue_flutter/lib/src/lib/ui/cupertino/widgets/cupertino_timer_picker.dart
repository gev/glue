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
