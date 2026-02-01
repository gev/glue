import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// DatePickerDialog widget function
/// Creates Flutter DatePickerDialog from Glue (date-picker-dialog props) expressions
final Ir datePickerDialog = IrNativeFunc(datePickerDialogImpl);

/// DatePickerDialog implementation - takes properties object
Eval<Ir> datePickerDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDatePickerDialog(
    WidgetProperties(properties.unlock),
  ),
  _ => _createDatePickerDialog(WidgetProperties.empty()),
};

/// Create DatePickerDialog widget from properties
Eval<Ir> _createDatePickerDialog(WidgetProperties properties) {
  final firstDate = properties.getValue<DateTime>('first-date');
  if (firstDate == null) {
    return throwError(wrongArgumentType(['first-date']));
  }
  final lastDate = properties.getValue<DateTime>('last-date');
  if (lastDate == null) {
    return throwError(wrongArgumentType(['last-date']));
  }
  return getRuntime().map((runtime) {
    final datePickerDialogWidget = DatePickerDialog(
      key: properties.key,
      initialDate: properties.getValue<DateTime>('initial-date'),
      firstDate: firstDate,
      lastDate: lastDate,
      currentDate: properties.getValue<DateTime>('current-date'),
      initialEntryMode:
          properties.getValue<DatePickerEntryMode>('initial-entry-mode') ??
          DatePickerEntryMode.calendar,
      selectableDayPredicate: properties.getValue<SelectableDayPredicate>(
        'selectable-day-predicate',
      ),
      cancelText: properties.getString('cancel-text'),
      confirmText: properties.getString('confirm-text'),
      helpText: properties.getString('help-text'),
      errorFormatText: properties.getString('error-format-text'),
      errorInvalidText: properties.getString('error-invalid-text'),
      fieldHintText: properties.getString('field-hint-text'),
      fieldLabelText: properties.getString('field-label-text'),
      keyboardType: properties.getValue<TextInputType>('keyboard-type'),
      restorationId: properties.getString('restoration-id'),
      onDatePickerModeChange: properties
          .getCallback<DatePickerEntryMode>('on-date-picker-mode-change')
          ?.call(runtime),
      switchToInputEntryModeIcon: properties.getValue<Icon>(
        'switch-to-input-entry-mode-icon',
      ),
      switchToCalendarEntryModeIcon: properties.getValue<Icon>(
        'switch-to-calendar-entry-mode-icon',
      ),
      insetPadding:
          properties.getValue<EdgeInsets>('inset-padding') ??
          EdgeInsets.symmetric(horizontal: 16.0, vertical: 24.0),
      calendarDelegate:
          properties.getValue<CalendarDelegate>('calendar-delegate') ??
          const GregorianCalendarDelegate(),
    );
    return IrNativeValue(Value(datePickerDialogWidget));
  });
}
