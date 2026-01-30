import 'package:flutter/material.dart';
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
  final datePickerDialogWidget = DatePickerDialog(
    key: properties.key,
    initialDate: properties.getValue('initial-date') as DateTime,
    firstDate: properties.getValue('first-date') as DateTime,
    lastDate: properties.getValue('last-date') as DateTime,
    currentDate: properties.getValue('current-date') as DateTime?,
    initialEntryMode: properties.getValue('initial-entry-mode'),
    selectableDayPredicate: properties.getValue('selectable-day-predicate'),
    cancelText: properties.getString('cancel-text'),
    confirmText: properties.getString('confirm-text'),
    helpText: properties.getString('help-text'),
    errorFormatText: properties.getString('error-format-text'),
    errorInvalidText: properties.getString('error-invalid-text'),
    fieldHintText: properties.getString('field-hint-text'),
    fieldLabelText: properties.getString('field-label-text'),
    keyboardType: properties.getValue('keyboard-type'),
    restorationId: properties.getString('restoration-id'),
    onDatePickerModeChange: properties.getValue('on-date-picker-mode-change'),
    switchToInputEntryModeIcon: properties.getValue(
      'switch-to-input-entry-mode-icon',
    ),
    switchToCalendarEntryModeIcon: properties.getValue(
      'switch-to-calendar-entry-mode-icon',
    ),
    insetPadding: properties.getValue('inset-padding') as EdgeInsets,
    calendarDelegate: properties.getValue('calendar-delegate'),
  );
  return Eval.pure(IrNativeValue(Value(datePickerDialogWidget)));
}
