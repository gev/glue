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
      key: properties.key,
      value: properties.getValue<Object>('value'),
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      toggleable: properties.getBool('toggleable') ?? false,
      activeColor: properties.getColor('active-color'),
      fillColor: properties.getValue<WidgetStateProperty<Color?>>('fill-color'),
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      overlayColor: properties.getValue<WidgetStateProperty<Color?>>(
        'overlay-color',
      ),
      splashRadius: properties.getDouble('splash-radius'),
      materialTapTargetSize: properties.getValue<MaterialTapTargetSize>(
        'material-tap-target-size',
      ),
      visualDensity: properties.getValue<VisualDensity>('visual-density'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      enabled: properties.getBool('enabled'),
      groupRegistry: properties.getValue<RadioGroupRegistry<Object>>(
        'group-registry',
      ),
      backgroundColor: properties.getValue<WidgetStateProperty<Color?>>(
        'background-color',
      ),
      side: properties.getValue<BorderSide?>('side'),
      innerRadius: properties.getValue<WidgetStateProperty<double?>>(
        'inner-radius',
      ),
    );
    return IrNativeValue(Value(radioWidget));
  });
}
