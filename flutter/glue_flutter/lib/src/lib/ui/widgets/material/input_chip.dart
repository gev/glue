import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// InputChip widget function
/// Creates Flutter InputChip from Glue (input-chip props) expressions
final Ir inputChip = IrNativeFunc(inputChipImpl);

/// InputChip implementation - takes properties object
Eval<Ir> inputChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createInputChip(
    Properties(properties.unlock),
  ),
  _ => _createInputChip(Properties.empty()),
};

/// Create InputChip widget from properties
Eval<Ir> _createInputChip(Properties properties) {
  return getRuntime().map((runtime) {
    final inputChipWidget = InputChip(
      selected: properties.inputChipSelected,
      isEnabled: properties.inputChipIsEnabled,
      label: properties.inputChipLabel ?? const Text(''),
      labelStyle: properties.inputChipLabelStyle,
      labelPadding: properties.inputChipLabelPadding,
      deleteIcon: properties.inputChipDeleteIcon,
      onDeleted: properties.inputChipOnDeleted(runtime),
      deleteIconColor: properties.inputChipDeleteIconColor,
      deleteButtonTooltipMessage:
          properties.inputChipDeleteButtonTooltipMessage,
      onSelected: properties.inputChipOnSelected,
      onPressed: properties.inputChipOnPressed(runtime),
      pressElevation: properties.inputChipPressElevation,
      avatar: properties.inputChipAvatar,
      avatarBoxConstraints: properties.inputChipAvatarBoxConstraints,
      side: properties.inputChipSide,
      shape: properties.inputChipShape,
      clipBehavior: properties.inputChipClipBehavior,
      focusNode: properties.inputChipFocusNode,
      autofocus: properties.inputChipAutofocus,
      backgroundColor: properties.inputChipBackgroundColor,
      disabledColor: properties.inputChipDisabledColor,
      selectedColor: properties.inputChipSelectedColor,
      checkmarkColor: properties.inputChipCheckmarkColor,
      showCheckmark: properties.inputChipShowCheckmark,
    );
    return IrNativeValue(Value(inputChipWidget));
  });
}
