import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Radio widget function
/// Creates Flutter Radio from Glue (radio props) expressions
final Ir radio = IrNativeFunc(radioImpl);

/// Radio implementation - takes properties object
Eval<Ir> radioImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRadio(
    MaterialProperties(properties.unlock),
  ),
  _ => _createRadio(MaterialProperties.empty()),
};

/// Create Radio widget from properties
Eval<Ir> _createRadio(MaterialProperties properties) {
  final radioWidget = Radio(
    value: properties.radioValue,
    groupValue: properties.radioGroupValue,
    onChanged: properties.onRadioChanged,
    mouseCursor: properties.mouseCursor,
    toggleable: properties.toggleable,
    activeColor: properties.activeColor,
    fillColor: properties.radioFillColor,
    focusColor: properties.focusColor,
    hoverColor: properties.hoverColor,
    overlayColor: properties.overlayColor,
    splashRadius: properties.splashRadius,
    materialTapTargetSize: properties.materialTapTargetSize,
    visualDensity: properties.visualDensity,
    focusNode: properties.focusNode,
    autofocus: properties.autofocus,
  );
  return Eval.pure(IrNativeValue(Value(radioWidget)));
}
