import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final onChanged = properties.getCallback<bool>('on-changed');
  if (onChanged == null) {
    return throwError(wrongArgumentType(['on-changed']));
  }
  return getRuntime().map((runtime) {
    final checkboxWidget = Checkbox(
      key: properties.key,
      value: properties.getBool('value'),
      tristate: properties.getBool('tristate') ?? false,
      onChanged: onChanged(runtime),
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      activeColor: properties.getColor('active-color'),
      fillColor: properties.getValue<WidgetStateProperty<Color?>>('fill-color'),
      checkColor: properties.getColor('check-color'),
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
      shape: properties.getValue<OutlinedBorder>('shape'),
      side: properties.getValue<BorderSide>('side'),
      isError: properties.getBool('is-error') ?? false,
      semanticLabel: properties.getString('semantic-label'),
    );
    return IrNativeValue(Value(checkboxWidget));
  });
}
