import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final label = properties.getWidget('label');
  if (label == null) {
    return throwError(wrongArgumentType(['choice-chip-label']));
  }
  return getRuntime().map((runtime) {
    final choiceChipWidget = ChoiceChip(
      selected: properties.getBool('choice-chip-selected') ?? false,
      label: label,
      labelStyle: properties.getValue<TextStyle>('choice-chip-label-style'),
      labelPadding: properties.getValue<EdgeInsetsGeometry>(
        'choice-chip-label-padding',
      ),
      avatar: properties.getValue<Widget>('choice-chip-avatar'),
      avatarBoxConstraints: properties.getValue<BoxConstraints>(
        'choice-chip-avatar-box-constraints',
      ),
      onSelected: properties
          .getCallback<bool>('choice-chip-on-selected')
          ?.call(runtime),
      pressElevation: properties.getValue<double>(
        'choice-chip-press-elevation',
      ),
      side: properties.getValue<BorderSide>('choice-chip-side'),
      shape: properties.getValue<RoundedRectangleBorder>('choice-chip-shape'),
      clipBehavior:
          properties.getValue<Clip>('choice-chip-clip-behavior') ?? Clip.none,
      focusNode: properties.getValue<FocusNode>('choice-chip-focus-node'),
      autofocus: properties.getBool('choice-chip-autofocus') ?? false,
      backgroundColor: properties.getColor('choice-chip-background-color'),
      disabledColor: properties.getColor('choice-chip-disabled-color'),
      selectedColor: properties.getColor('choice-chip-selected-color'),
      checkmarkColor: properties.getColor('choice-chip-checkmark-color'),
      showCheckmark: properties.getBool('choice-chip-show-checkmark') ?? false,
    );
    return IrNativeValue(Value(choiceChipWidget));
  });
}
