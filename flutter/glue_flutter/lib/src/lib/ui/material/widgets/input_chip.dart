import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// InputChip widget function
/// Creates Flutter InputChip from Glue (input-chip props) expressions
final Ir inputChip = IrNativeFunc(inputChipImpl);

/// InputChip implementation - takes properties object
Eval<Ir> inputChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createInputChip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createInputChip(WidgetProperties.empty()),
};

/// Create InputChip widget from properties
Eval<Ir> _createInputChip(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final inputChipWidget = InputChip(
      selected: properties.getBool('selected') ?? false,
      isEnabled: properties.getBool('is-enabled') ?? true,
      label: properties.child ?? const Text('InputChip'),
      labelStyle: properties.getValue('label-style'),
      labelPadding: properties.getValue('label-padding'),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted', runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      onSelected: properties.getValue('on-selected'),
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      pressElevation: properties.getDouble('press-elevation'),
      avatar: properties.getWidget('avatar'),
      avatarBoxConstraints: properties.getValue('avatar-box-constraints'),
      side: properties.getValue('border-side'),
      shape: properties.getValue('outlined-border'),
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      backgroundColor: properties.getColor('background-color'),
      disabledColor: properties.getColor('disabled-color'),
      selectedColor: properties.getColor('selected-color'),
      checkmarkColor: properties.getColor('checkmark-color'),
      showCheckmark: properties.getBool('show-checkmark'),
    );
    return IrNativeValue(Value(inputChipWidget));
  });
}
