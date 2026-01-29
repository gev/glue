import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Badge widget function
/// Creates Flutter Badge from Glue (badge props) expressions
final Ir badge = IrNativeFunc(badgeImpl);

/// Badge implementation - takes properties object
Eval<Ir> badgeImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBadge(
    MaterialProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Badge widget from properties
Eval<Ir> _createBadge(MaterialProperties properties) {
  if (properties.child == null) {
    throwError(wrongArgumentType(['child property required']));
  }

  final badgeWidget = Badge(
    child: properties.child!,
    label: properties.badgeLabel,
    backgroundColor: properties.color, // using color for background
    textColor: properties.foregroundColor, // using foregroundColor for text
    textStyle: properties.textStyle,
    padding: properties.padding,
    alignment:
        properties.fabAlignment, // using fabAlignment for badge alignment
    isLabelVisible: properties.isLabelVisible ?? true,
    offset: properties.badgeOffset ?? Offset.zero,
  );
  return Eval.pure(IrNativeValue(Value(badgeWidget)));
}
