import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ChoiceChip widget function
/// Creates Flutter ChoiceChip from Glue (choice-chip props) expressions
final Ir choiceChip = IrNativeFunc(choiceChipImpl);

/// ChoiceChip implementation - takes properties object
Eval<Ir> choiceChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createChoiceChip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createChoiceChip(WidgetProperties.empty()),
};

/// Create ChoiceChip widget from properties
Eval<Ir> _createChoiceChip(WidgetProperties properties) {
  final choiceChipWidget = ChoiceChip(
    selected: properties.getBool('choice-chip-selected') ?? false,
    label: properties.getValue<>('choice-chip-label') ?? const Text(''),
    labelStyle: properties.getValue<>('choice-chip-label-style'),
    labelPadding: properties.getValue<>('choice-chip-label-padding'),
    avatar: properties.getValue<>('choice-chip-avatar'),
    avatarBoxConstraints: properties.getValue<>(
      'choice-chip-avatar-box-constraints',
    ),
    onSelected: properties.getValue<>('choice-chip-on-selected'),
    pressElevation: properties.getValue<>('choice-chip-press-elevation'),
    side: properties.getValue<>('choice-chip-side'),
    shape: properties.getValue<>('choice-chip-shape'),
    clipBehavior: properties.getValue<>('choice-chip-clip-behavior'),
    focusNode: properties.getValue<>('choice-chip-focus-node'),
    autofocus: properties.getBool('choice-chip-autofocus') ?? false,
    backgroundColor: properties.getValue<>('choice-chip-background-color'),
    disabledColor: properties.getValue<>('choice-chip-disabled-color'),
    selectedColor: properties.getValue<>('choice-chip-selected-color'),
    checkmarkColor: properties.getValue<>('choice-chip-checkmark-color'),
    showCheckmark: properties.getBool('choice-chip-show-checkmark') ?? false,
  );
  return Eval.pure(IrNativeValue(Value(choiceChipWidget)));
}
