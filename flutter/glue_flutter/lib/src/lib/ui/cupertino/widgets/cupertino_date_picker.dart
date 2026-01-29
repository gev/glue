import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoDatePicker widget function
/// Creates Flutter CupertinoDatePicker from Glue expressions
/// Expects keyword arguments: :mode, :on-date-time-changed, etc.
final Ir cupertinoDatePicker = IrNativeFunc(cupertinoDatePickerImpl);

/// CupertinoDatePicker implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoDatePickerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoDatePicker(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoDatePicker(WidgetProperties.empty()),
};

/// Create CupertinoDatePicker widget from properties object
Eval<Ir> _createCupertinoDatePicker(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final initialDateTime =
        properties.getValue('cupertino-date-picker-initial-date-time') ??
        DateTime.now();

    final pickerWidget = CupertinoDatePicker(
      key: GlobalKey(),
      mode: properties.getValue('cupertino-date-picker-mode'),
      onDateTimeChanged: properties.getValue(
        'cupertino-date-picker-on-date-time-changed',
      ),
      initialDateTime: initialDateTime,
      minimumDate: properties.getValue('cupertino-date-picker-minimum-date'),
      maximumDate: properties.getValue('cupertino-date-picker-maximum-date'),
      minimumYear: properties.getValue('cupertino-date-picker-minimum-year'),
      maximumYear: properties.getValue('cupertino-date-picker-maximum-year'),
      minuteInterval: properties.getValue(
        'cupertino-date-picker-minute-interval',
      ),
      use24hFormat: properties.getValue('cupertino-date-picker-use-24h-format'),
      dateOrder: properties.getValue('cupertino-date-picker-date-order'),
      backgroundColor: properties.getValue(
        'cupertino-date-picker-background-color',
      ),
      showDayOfWeek: properties.getValue(
        'cupertino-date-picker-show-day-of-week',
      ),
      itemExtent: properties.getValue('cupertino-date-picker-item-extent'),
    );
    return IrNativeValue(Value(pickerWidget));
  });
}
