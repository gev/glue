import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// DatePickerDialog widget function
/// Creates Flutter DatePickerDialog from Glue (date-picker-dialog props) expressions
final Ir datePickerDialog = IrNativeFunc(datePickerDialogImpl);

/// DatePickerDialog implementation - takes properties object
Eval<Ir> datePickerDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDatePickerDialog(
    Properties(properties.unlock),
  ),
  _ => _createDatePickerDialog(Properties.empty()),
};

/// Create DatePickerDialog widget from properties
Eval<Ir> _createDatePickerDialog(Properties properties) {
  final datePickerDialogWidget = DatePickerDialog(
    initialDate: properties.datePickerInitialDate,
    firstDate: properties.datePickerFirstDate!,
    lastDate: properties.datePickerLastDate!,
    currentDate: properties.datePickerCurrentDate,
    initialEntryMode: properties.datePickerInitialEntryMode,
    selectableDayPredicate: properties.datePickerSelectableDayPredicate,
    cancelText: properties.datePickerCancelText,
    confirmText: properties.datePickerConfirmText,
    helpText: properties.datePickerHelpText,
    initialCalendarMode: properties.datePickerInitialCalendarMode,
    errorFormatText: properties.datePickerErrorFormatText,
    errorInvalidText: properties.datePickerErrorInvalidText,
    fieldHintText: properties.datePickerFieldHintText,
    fieldLabelText: properties.datePickerFieldLabelText,
    keyboardType: properties.datePickerKeyboardType,
    restorationId: properties.datePickerRestorationId,
    onDatePickerModeChange: properties.datePickerOnDatePickerModeChange,
    switchToInputEntryModeIcon: properties.datePickerSwitchToInputEntryModeIcon,
    switchToCalendarEntryModeIcon:
        properties.datePickerSwitchToCalendarEntryModeIcon,
    insetPadding: properties.datePickerInsetPadding,
  );
  return Eval.pure(IrNativeValue(Value(datePickerDialogWidget)));
}
