import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoDatePicker widget function
/// Creates Flutter CupertinoDatePicker from Glue expressions
/// Expects keyword arguments: :mode, :on-date-time-changed, etc.
final Ir cupertinoDatePicker = IrNativeFunc(cupertinoDatePickerImpl);

/// CupertinoDatePicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoDatePickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoDatePicker(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoDatePicker(CupertinoProperties.empty()),
};

/// Create CupertinoDatePicker widget from properties object
Eval<Ir> _createCupertinoDatePicker(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final callback = (DateTime value) =>
        properties.cupertinoDatePickerOnDateTimeChanged(value);

    final initialDateTime =
        properties.cupertinoDatePickerInitialDateTime ?? DateTime.now();

    final pickerWidget = CupertinoDatePicker(
      key: GlobalKey(),
      mode: properties.cupertinoDatePickerMode,
      onDateTimeChanged: callback,
      initialDateTime: initialDateTime,
      minimumDate: properties.cupertinoDatePickerMinimumDate,
      maximumDate: properties.cupertinoDatePickerMaximumDate,
      minimumYear: properties.cupertinoDatePickerMinimumYear,
      maximumYear: properties.cupertinoDatePickerMaximumYear,
      minuteInterval: properties.cupertinoDatePickerMinuteInterval,
      use24hFormat: properties.cupertinoDatePickerUse24hFormat,
      dateOrder: properties.cupertinoDatePickerDateOrder,
      backgroundColor: properties.cupertinoDatePickerBackgroundColor,
      showDayOfWeek: properties.cupertinoDatePickerShowDayOfWeek,
      itemExtent: properties.cupertinoDatePickerItemExtent,
    );
    return IrNativeValue(Value(pickerWidget));
  });
}
