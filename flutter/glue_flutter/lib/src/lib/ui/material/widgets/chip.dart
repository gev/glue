import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Chip widget function
/// Creates Flutter Chip from Glue (chip props) expressions
final Ir chip = IrNativeFunc(chipImpl);

/// Chip implementation - takes properties object
Eval<Ir> chipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createChip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createChip(WidgetProperties.empty()),
};

/// Create Chip widget from properties
Eval<Ir> _createChip(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final chipWidget = Chip(
      avatar: properties.getWidget('avatar'),
      label: properties.child ?? const Text('Chip'),
      labelStyle: properties.getValue('label-style'),
      labelPadding: properties.getValue('label-padding'),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted', runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      side: properties.getValue('border-side'),
      shape: properties.getValue('outlined-border'),
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      backgroundColor: properties.getColor('background-color'),
      padding: properties.getValue('padding'),
      visualDensity: properties.getValue('visual-density'),
      materialTapTargetSize: properties.getValue('material-tap-target-size'),
      elevation: properties.getDouble('elevation'),
      shadowColor: properties.getColor('shadow-color'),
      surfaceTintColor: properties.getColor('surface-tint-color'),
      iconTheme: properties.getValue('icon-theme'),
      avatarBoxConstraints: properties.getValue('avatar-box-constraints'),
      deleteIconBoxConstraints: properties.getValue(
        'delete-icon-box-constraints',
      ),
    );
    return IrNativeValue(Value(chipWidget));
  });
}
