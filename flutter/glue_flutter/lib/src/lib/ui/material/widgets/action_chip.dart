import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final label = properties.getWidget('label');
  if (label == null) {
    return throwError(
      wrongArgumentType([
        'ActionChip requires non-null "label" property of type Widget.',
      ]),
    );
  }
  return getRuntime().map((runtime) {
    final actionChipWidget = ActionChip(
      key: properties.key,
      label: label,
      labelStyle: properties.getValue<TextStyle>('label-style'),
      labelPadding: properties.getValue<EdgeInsets>('label-padding'),
      avatar: properties.getValue<Widget>('avatar'),
      avatarBoxConstraints: properties.getValue<BoxConstraints>(
        'avatar-box-constraints',
      ),
      onPressed: properties.getVoidCallback('on-pressed')?.call(runtime),
      pressElevation: properties.getDouble('press-elevation'),
      side: properties.getValue<BorderSide>('side'),
      shape: properties.getValue<OutlinedBorder>('shape'),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      backgroundColor: properties.getColor('background-color'),
      disabledColor: properties.getColor('disabled-color'),
    );
    return IrNativeValue(Value(actionChipWidget));
  });
}
