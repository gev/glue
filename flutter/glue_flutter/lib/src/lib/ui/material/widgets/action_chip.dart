import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ActionChip widget function
/// Creates Flutter ActionChip from Glue (action-chip props) expressions
final Ir actionChip = IrNativeFunc(actionChipImpl);

/// ActionChip implementation - takes properties object
Eval<Ir> actionChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createActionChip(
    WidgetProperties(properties.unlock),
  ),
  _ => _createActionChip(WidgetProperties.empty()),
};

/// Create ActionChip widget from properties
Eval<Ir> _createActionChip(WidgetProperties properties) {
  // Get runtime and create widget
  return getRuntime().map((runtime) {
    final actionChipWidget = ActionChip(
      key: properties.key,
      label: properties.getValue<>('label') ?? const Text(''),
      labelStyle: properties.getValue<>('label-style'),
      labelPadding: properties.getValue<>('label-padding'),
      avatar: properties.getValue<>('avatar'),
      avatarBoxConstraints: properties.getValue<>('avatar-box-constraints'),
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      pressElevation: properties.getValue<>('press-elevation'),
      side: properties.getValue<>('side'),
      shape: properties.getValue<>('shape'),
      clipBehavior: properties.getValue<>('clip-behavior'),
      focusNode: properties.getValue<>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      backgroundColor: properties.getColor('background-color'),
      disabledColor: properties.getColor('disabled-color'),
    );
    return IrNativeValue(Value(actionChipWidget));
  });
}
