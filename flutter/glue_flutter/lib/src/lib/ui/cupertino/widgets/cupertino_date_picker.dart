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
  final initialDateTime =
      properties.getValue('initial-date-time') ?? DateTime.now();

  final pickerWidget = CupertinoDatePicker(
    key: properties.key,
    mode: properties.getValue('mode'),
    onDateTimeChanged: properties.getValue('on-date-time-changed'),
    initialDateTime: initialDateTime,
    minimumDate: properties.getValue('minimum-date'),
    maximumDate: properties.getValue('maximum-date'),
    minimumYear: properties.getInt('minimum-year') ?? 1,
    maximumYear: properties.getInt('maximum-year'),
    minuteInterval: properties.getInt('minute-interval') ?? 1,
    use24hFormat: properties.getBool('use24h-format') ?? false,
    dateOrder: properties.getValue('date-order'),
    backgroundColor: properties.getColor('background-color'),
    showDayOfWeek: properties.getBool('show-day-of-week') ?? false,
    itemExtent: properties.getDouble('item-extent') ?? 44.0,
  );
  return Eval.pure(IrNativeValue(Value(pickerWidget)));
}
