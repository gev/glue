import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ElevatedButton widget function
/// Creates Flutter ElevatedButton from Glue (elevated-button props) expressions
final Ir elevatedButton = IrNativeFunc(elevatedButtonImpl);

/// ElevatedButton implementation - takes properties object
Eval<Ir> elevatedButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createElevatedButton(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create ElevatedButton widget from properties
Eval<Ir> _createElevatedButton(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final elevatedButtonWidget = ElevatedButton(
      key: properties.key,
      onPressed: properties.getVoidCallback('on-pressed', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onHover: properties.getValue<>('on-hover'),
      onFocusChange: properties.getValue<>('on-focus-change'),
      style: properties.getValue<>('style'),
      focusNode: properties.getValue<>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      clipBehavior: properties.getValue<>('clip-behavior') ?? Clip.none,
      child: properties.child,
    );
    return IrNativeValue(Value(elevatedButtonWidget));
  });
}
