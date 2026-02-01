import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Badge widget function
/// Creates Flutter Badge from Glue (badge props) expressions
final Ir badge = IrNativeFunc(badgeImpl);

/// Badge implementation - takes properties object
Eval<Ir> badgeImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBadge(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Badge widget from properties
Eval<Ir> _createBadge(WidgetProperties properties) {
  if (properties.child == null) {
    throwError(wrongArgumentType(['child property required']));
  }

  final badgeWidget = Badge(
    key: properties.key,
    child: properties.child!,
    label: properties.getWidget('label'),
    backgroundColor: properties.getColor('background-color'),
    textColor: properties.getColor('text-color'),
    textStyle: properties.getValue<TextStyle>('text-style'),
    padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    alignment: properties.getValue<AlignmentGeometry>('alignment'),
    isLabelVisible: properties.getBool('is-label-visible') ?? true,
    largeSize: properties.getDouble('large-size'),
    offset: properties.getValue<Offset>('offset'),
  );
  return Eval.pure(IrNativeValue(Value(badgeWidget)));
}
