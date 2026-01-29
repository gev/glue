import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Radio widget function
/// Creates Flutter Radio from Glue (radio props) expressions
final Ir radio = IrNativeFunc(radioImpl);

/// Radio implementation - takes properties object
Eval<Ir> radioImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRadio(
    WidgetProperties(properties.unlock),
  ),
  _ => _createRadio(WidgetProperties.empty()),
};

/// Create Radio widget from properties
Eval<Ir> _createRadio(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final radioWidget = Radio(
      value: properties.getValue('value'),
      groupValue: properties.getValue('group-value'),
      onChanged: properties.getValue('on-changed') as ValueChanged<dynamic>?,
      mouseCursor: properties.getValue('mouse-cursor'),
      toggleable: properties.getBool('toggleable') ?? false,
      activeColor: properties.getColor('active-color'),
      fillColor: properties.getValue('fill-color'),
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      overlayColor: properties.getValue('overlay-color'),
      splashRadius: properties.getDouble('splash-radius'),
      materialTapTargetSize: properties.getValue('material-tap-target-size'),
      visualDensity: properties.getValue('visual-density'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
    );
    return IrNativeValue(Value(radioWidget));
  });
}
