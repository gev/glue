import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Checkbox widget function
/// Creates Flutter Checkbox from Glue (checkbox props) expressions
final Ir checkbox = IrNativeFunc(checkboxImpl);

/// Checkbox implementation - takes properties object
Eval<Ir> checkboxImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCheckbox(
    MaterialProperties(properties.unlock),
  ),
  _ => _createCheckbox(MaterialProperties.empty()),
};

/// Create Checkbox widget from properties
Eval<Ir> _createCheckbox(MaterialProperties properties) {
  final checkboxWidget = Checkbox(
    value: properties.checkboxValue,
    tristate: properties.tristate,
    onChanged: properties.onCheckboxChanged,
    mouseCursor: properties.mouseCursor,
    activeColor: properties.activeColor,
    fillColor: properties.fillColor,
    checkColor: properties.checkColor,
    focusColor: properties.focusColor,
    hoverColor: properties.hoverColor,
    overlayColor: properties.overlayColor,
    splashRadius: properties.splashRadius,
    materialTapTargetSize: properties.materialTapTargetSize,
    visualDensity: properties.visualDensity,
    focusNode: properties.focusNode,
    autofocus: properties.autofocus,
    shape: properties.checkboxShape,
    side: properties.checkboxSide,
    isError: properties.isError,
    semanticLabel: properties.checkboxSemanticLabel,
  );
  return Eval.pure(IrNativeValue(Value(checkboxWidget)));
}
