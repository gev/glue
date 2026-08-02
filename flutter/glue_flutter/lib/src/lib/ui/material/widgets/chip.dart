import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final label = properties.getWidget('label');
  if (label == null) {
    return throwError(wrongArgumentType(['Property `label` required']));
  }
  return getRuntime().map((runtime) {
    final chipWidget = Chip(
      key: properties.key,
      avatar: properties.getWidget('avatar'),
      label: label,
      labelStyle: properties.getValue<TextStyle>('label-style'),
      labelPadding: properties.getValue<EdgeInsetsGeometry>('label-padding'),
      deleteIcon: properties.getWidget('delete-icon'),
      onDeleted: properties.getVoidCallback('on-deleted')?.call(runtime),
      deleteIconColor: properties.getColor('delete-icon-color'),
      deleteButtonTooltipMessage: properties.getString(
        'delete-button-tooltip-message',
      ),
      side: properties.getValue<BorderSide>('side'),
      shape: properties.getValue<RoundedRectangleBorder>('shape'),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      color: WidgetStateProperty.all(properties.getValue<Color?>('color')),
      backgroundColor: properties.getColor('background-color'),
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
      visualDensity: properties.getValue<VisualDensity>('visual-density'),
      materialTapTargetSize: properties.getValue<MaterialTapTargetSize>(
        'material-tap-target-size',
      ),
      elevation: properties.getDouble('elevation'),
      shadowColor: properties.getColor('shadow-color'),
      surfaceTintColor: properties.getColor('surface-tint-color'),
      iconTheme: properties.getValue<IconThemeData>('icon-theme'),
      avatarBoxConstraints: properties.getValue<BoxConstraints>(
        'avatar-box-constraints',
      ),
      deleteIconBoxConstraints: properties.getValue<BoxConstraints>(
        'delete-icon-box-constraints',
      ),
      chipAnimationStyle: properties.getValue<ChipAnimationStyle>(
        'chip-animation-style',
      ),
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
    );
    return IrNativeValue(Value(chipWidget));
  });
}
