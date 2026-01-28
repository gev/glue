import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ActionChip widget function
/// Creates Flutter ActionChip from Glue (action-chip props) expressions
final Ir actionChip = IrNativeFunc(actionChipImpl);

/// ActionChip implementation - takes properties object
Eval<Ir> actionChipImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createActionChip(
    Properties(properties.unlock),
  ),
  _ => _createActionChip(Properties.empty()),
};

/// Create ActionChip widget from properties
Eval<Ir> _createActionChip(Properties properties) {
  final actionChipWidget = ActionChip(
    label: properties.actionChipLabel ?? const Text(''),
    labelStyle: properties.actionChipLabelStyle,
    labelPadding: properties.actionChipLabelPadding,
    avatar: properties.actionChipAvatar,
    avatarBoxConstraints: properties.actionChipAvatarBoxConstraints,
    onPressed: properties.actionChipOnPressed,
    pressElevation: properties.actionChipPressElevation,
    side: properties.actionChipSide,
    shape: properties.actionChipShape,
    clipBehavior: properties.actionChipClipBehavior,
    focusNode: properties.actionChipFocusNode,
    autofocus: properties.actionChipAutofocus,
    backgroundColor: properties.actionChipBackgroundColor,
    disabledColor: properties.actionChipDisabledColor,
  );
  return Eval.pure(IrNativeValue(Value(actionChipWidget)));
}
