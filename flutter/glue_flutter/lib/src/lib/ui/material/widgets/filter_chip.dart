import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FilterChip widget function
/// Creates Flutter FilterChip from Glue (filter-chip props) expressions
final Ir filterChip = IrNativeFunc(filterChipImpl);

/// FilterChip implementation - takes properties object
Eval<Ir> filterChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFilterChip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createFilterChip(WidgetProperties.empty()),
};

/// Create FilterChip widget from properties
Eval<Ir> _createFilterChip(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final filterChipWidget = FilterChip(
      selected: properties.getBool('selected') ?? false,
      label: properties.child ?? const Text('FilterChip'),
      labelStyle: properties.getValue<>('label-style'),
      labelPadding: properties.getValue<>('label-padding'),
      avatar: properties.getWidget('avatar'),
      avatarBoxConstraints: properties.getValue<>('avatar-box-constraints'),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted', runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      onSelected: properties.getValue<>('on-selected'),
      pressElevation: properties.getDouble('press-elevation'),
      side: properties.getValue<>('border-side'),
      shape: properties.getValue<>('outlined-border'),
      clipBehavior: properties.getValue<>('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue<>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      backgroundColor: properties.getColor('background-color'),
      disabledColor: properties.getColor('disabled-color'),
      selectedColor: properties.getColor('selected-color'),
      checkmarkColor: properties.getColor('checkmark-color'),
      showCheckmark: properties.getBool('show-checkmark'),
    );
    return IrNativeValue(Value(filterChipWidget));
  });
}
