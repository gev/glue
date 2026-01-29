import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// ChoiceChip widget function
/// Creates Flutter ChoiceChip from Glue (choice-chip props) expressions
final Ir choiceChip = IrNativeFunc(choiceChipImpl);

/// ChoiceChip implementation - takes properties object
Eval<Ir> choiceChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createChoiceChip(
    Properties(properties.unlock),
  ),
  _ => _createChoiceChip(Properties.empty()),
};

/// Create ChoiceChip widget from properties
Eval<Ir> _createChoiceChip(Properties properties) {
  final choiceChipWidget = ChoiceChip(
    selected: properties.choiceChipSelected,
    label: properties.choiceChipLabel ?? const Text(''),
    labelStyle: properties.choiceChipLabelStyle,
    labelPadding: properties.choiceChipLabelPadding,
    avatar: properties.choiceChipAvatar,
    avatarBoxConstraints: properties.choiceChipAvatarBoxConstraints,
    onSelected: properties.choiceChipOnSelected,
    pressElevation: properties.choiceChipPressElevation,
    side: properties.choiceChipSide,
    shape: properties.choiceChipShape,
    clipBehavior: properties.choiceChipClipBehavior,
    focusNode: properties.choiceChipFocusNode,
    autofocus: properties.choiceChipAutofocus,
    backgroundColor: properties.choiceChipBackgroundColor,
    disabledColor: properties.choiceChipDisabledColor,
    selectedColor: properties.choiceChipSelectedColor,
    checkmarkColor: properties.choiceChipCheckmarkColor,
    showCheckmark: properties.choiceChipShowCheckmark,
  );
  return Eval.pure(IrNativeValue(Value(choiceChipWidget)));
}
