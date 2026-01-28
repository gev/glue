import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TimePickerDialog widget function
/// Creates Flutter TimePickerDialog from Glue (time-picker-dialog props) expressions
final Ir timePickerDialog = IrNativeFunc(timePickerDialogImpl);

/// TimePickerDialog implementation - takes properties object
Eval<Ir> timePickerDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTimePickerDialog(
    Properties(properties.unlock),
  ),
  _ => _createTimePickerDialog(Properties.empty()),
};

/// Create TimePickerDialog widget from properties
Eval<Ir> _createTimePickerDialog(Properties properties) {
  final timePickerDialogWidget = TimePickerDialog(
    initialTime: properties.timePickerInitialTime,
    cancelText: properties.timePickerCancelText,
    confirmText: properties.timePickerConfirmText,
    helpText: properties.timePickerHelpText,
    errorInvalidText: properties.timePickerErrorInvalidText,
    hourLabelText: properties.timePickerHourLabelText,
    minuteLabelText: properties.timePickerMinuteLabelText,
    restorationId: properties.timePickerRestorationId,
    initialEntryMode: properties.timePickerInitialEntryMode,
    orientation: properties.timePickerOrientation,
    onEntryModeChanged: properties.timePickerOnEntryModeChanged,
    switchToInputEntryModeIcon: properties.timePickerSwitchToInputEntryModeIcon,
    switchToTimerEntryModeIcon: properties.timePickerSwitchToTimerEntryModeIcon,
    emptyInitialInput: properties.timePickerEmptyInitialInput,
  );
  return Eval.pure(IrNativeValue(Value(timePickerDialogWidget)));
}
