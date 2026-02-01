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
      labelStyle: properties.getValue<TextStyle>('label-style'),
      labelPadding: properties.getValue<EdgeInsetsGeometry>('label-padding'),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted')?.call(runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      onSelected: properties.getCallback<bool>('on-selected')?.call(runtime),
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
      pressElevation: properties.getDouble('press-elevation'),
      avatar: properties.getWidget('avatar'),
      avatarBoxConstraints: properties.getValue<BoxConstraints>(
        'avatar-box-constraints',
      ),
      side: properties.getValue<BorderSide>('border-side'),
      shape: properties.getValue<OutlinedBorder>('outlined-border'),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue<FocusNode>('focus-node'),
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
