import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Checkbox widget function
/// Creates Flutter Checkbox from Glue (checkbox props) expressions
final Ir checkbox = IrNativeFunc(checkboxImpl);

/// Checkbox implementation - takes properties object
Eval<Ir> checkboxImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCheckbox(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCheckbox(WidgetProperties.empty()),
};

/// Create Checkbox widget from properties
Eval<Ir> _createCheckbox(WidgetProperties properties) {
  final checkboxWidget = Checkbox(
    key: properties.key,
    value: properties.getValue('value') as bool?,
    tristate: properties.getBool('tristate') ?? false,
    onChanged: properties.getValue('on-changed'),
    mouseCursor: properties.getValue('mouse-cursor'),
    activeColor: properties.getColor('active-color'),
    fillColor: properties.getValue('fill-color'),
    checkColor: properties.getColor('check-color'),
    focusColor: properties.getColor('focus-color'),
    hoverColor: properties.getColor('hover-color'),
    overlayColor: properties.getValue('overlay-color'),
    splashRadius: properties.getDouble('splash-radius'),
    materialTapTargetSize: properties.getValue('material-tap-target-size'),
    visualDensity: properties.getValue('visual-density'),
    focusNode: properties.getValue('focus-node'),
    autofocus: properties.getBool('autofocus') ?? false,
    shape: properties.getValue('shape'),
    side: properties.getValue('side'),
    isError: properties.getBool('is-error') ?? false,
    semanticLabel: properties.getString('semantic-label'),
  );
  return Eval.pure(IrNativeValue(Value(checkboxWidget)));
}
