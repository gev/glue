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
      labelStyle: properties.getValue<TextStyle>('label-style'),
      labelPadding: properties.getValue<EdgeInsetsGeometry>('label-padding'),
      avatar: properties.getWidget('avatar'),
      avatarBoxConstraints: properties.getValue<BoxConstraints>(
        'avatar-box-constraints',
      ),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted')?.call(runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      onSelected: properties.getCallback<bool>('on-selected')?.call(runtime),
      pressElevation: properties.getDouble('press-elevation'),
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
    return IrNativeValue(Value(filterChipWidget));
  });
}
