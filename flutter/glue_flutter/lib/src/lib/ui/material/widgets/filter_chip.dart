import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// FilterChip widget function
/// Creates Flutter FilterChip from Glue (filter-chip props) expressions
final Ir filterChip = IrNativeFunc(filterChipImpl);

/// FilterChip implementation - takes properties object
Eval<Ir> filterChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFilterChip(
    MaterialProperties(properties.unlock),
  ),
  _ => _createFilterChip(MaterialProperties.empty()),
};

/// Create FilterChip widget from properties
Eval<Ir> _createFilterChip(MaterialProperties properties) {
  final filterChipWidget = FilterChip(
    selected: properties.filterChipSelected,
    label: properties.filterChipLabel ?? const Text(''),
    labelStyle: properties.filterChipLabelStyle,
    labelPadding: properties.filterChipLabelPadding,
    avatar: properties.filterChipAvatar,
    avatarBoxConstraints: properties.filterChipAvatarBoxConstraints,
    deleteIcon: properties.filterChipDeleteIcon,
    onDeleted: properties.filterChipOnDeleted,
    deleteIconColor: properties.filterChipDeleteIconColor,
    deleteButtonTooltipMessage: properties.filterChipDeleteButtonTooltipMessage,
    onSelected: properties.filterChipOnSelected,
    pressElevation: properties.filterChipPressElevation,
    side: properties.filterChipSide,
    shape: properties.filterChipShape,
    clipBehavior: properties.filterChipClipBehavior,
    focusNode: properties.filterChipFocusNode,
    autofocus: properties.filterChipAutofocus,
    backgroundColor: properties.filterChipBackgroundColor,
    disabledColor: properties.filterChipDisabledColor,
    selectedColor: properties.filterChipSelectedColor,
    checkmarkColor: properties.filterChipCheckmarkColor,
    showCheckmark: properties.filterChipShowCheckmark,
  );
  return Eval.pure(IrNativeValue(Value(filterChipWidget)));
}
