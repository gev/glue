import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TimePickerDialog widget function
/// Creates Flutter TimePickerDialog from Glue (time-picker-dialog props) expressions
final Ir timePickerDialog = IrNativeFunc(timePickerDialogImpl);

/// TimePickerDialog implementation - takes properties object
Eval<Ir> timePickerDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTimePickerDialog(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTimePickerDialog(WidgetProperties.empty()),
};

/// Create TimePickerDialog widget from properties
Eval<Ir> _createTimePickerDialog(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final timePickerDialogWidget = TimePickerDialog(
      key: properties.key,
      initialTime: properties.getValue('initial-time'),
      cancelText: properties.getString('cancel-text'),
      confirmText: properties.getString('confirm-text'),
      helpText: properties.getString('help-text'),
      errorInvalidText: properties.getString('error-invalid-text'),
      hourLabelText: properties.getString('hour-label-text'),
      minuteLabelText: properties.getString('minute-label-text'),
      restorationId: properties.getString('restoration-id'),
      initialEntryMode: properties.getValue('initial-entry-mode'),
      orientation: properties.getValue('orientation'),
      onEntryModeChanged: properties.getValue('on-entry-mode-changed'),
      switchToInputEntryModeIcon: properties.getValue(
        'switch-to-input-entry-mode-icon',
      ),
      switchToTimerEntryModeIcon: properties.getValue(
        'switch-to-timer-entry-mode-icon',
      ),
      emptyInitialInput: properties.getBool('empty-initial-input') ?? false,
    );
    return IrNativeValue(Value(timePickerDialogWidget));
  });
}
