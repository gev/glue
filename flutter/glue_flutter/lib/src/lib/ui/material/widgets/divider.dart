import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Divider widget function
/// Creates Flutter Divider from Glue (divider props) expressions
final Ir divider = IrNativeFunc(dividerImpl);

/// Divider implementation - takes properties object
Eval<Ir> dividerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDivider(
    MaterialProperties(properties.unlock),
  ),
  _ => _createDivider(MaterialProperties.empty()),
};

/// Create Divider widget from properties
Eval<Ir> _createDivider(MaterialProperties properties) {
  final dividerWidget = Divider(
    height: properties.dividerHeight,
    thickness: properties.dividerThickness,
    indent: properties.dividerIndent,
    endIndent: properties.dividerEndIndent,
    color: properties.color,
  );
  return Eval.pure(IrNativeValue(Value(dividerWidget)));
}
