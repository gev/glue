import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Chip widget function
/// Creates Flutter Chip from Glue (chip props) expressions
final Ir chip = IrNativeFunc(chipImpl);

/// Chip implementation - takes properties object
Eval<Ir> chipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createChip(Properties(properties.unlock)),
  _ => _createChip(Properties.empty()),
};

/// Create Chip widget from properties
Eval<Ir> _createChip(Properties properties) {
  final chipWidget = Chip(
    avatar: properties.chipAvatar,
    label:
        properties.child ??
        const Text('Chip'), // Use child as label, fallback to 'Chip'
    labelStyle: properties.chipLabelStyle,
    labelPadding: properties.chipLabelPadding,
    deleteIcon: properties.chipDeleteIcon,
    onDeleted: properties.chipOnDeleted,
    deleteIconColor: properties.chipDeleteIconColor,
    deleteButtonTooltipMessage: properties.chipDeleteButtonTooltipMessage,
    side: properties.chipSide,
    shape: properties.chipShape,
    clipBehavior: properties.clipBehavior,
    focusNode: properties.focusNode,
    autofocus: properties.autofocus,
    color: properties.chipColor,
    backgroundColor: properties.color, // using color for background
    padding: properties.chipPadding,
    visualDensity: properties.visualDensity,
    materialTapTargetSize: properties.materialTapTargetSize,
    elevation: properties.dividerThickness, // using thickness for elevation
    shadowColor: properties.shadowColor,
    surfaceTintColor: properties.surfaceTintColor,
    iconTheme: properties.iconTheme,
    avatarBoxConstraints: properties.chipAvatarBoxConstraints,
    deleteIconBoxConstraints: properties.chipDeleteIconBoxConstraints,
  );
  return Eval.pure(IrNativeValue(Value(chipWidget)));
}
