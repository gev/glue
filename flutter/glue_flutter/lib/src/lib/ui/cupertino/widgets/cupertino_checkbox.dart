import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoCheckbox widget function
/// Creates Flutter CupertinoCheckbox from Glue expressions
/// Expects keyword arguments: :value, :on-changed, etc.
final Ir cupertinoCheckbox = IrNativeFunc(cupertinoCheckboxImpl);

/// CupertinoCheckbox implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoCheckboxImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoCheckbox(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoCheckbox(WidgetProperties.empty()),
};

/// Create CupertinoCheckbox widget from properties object
Eval<Ir> _createCupertinoCheckbox(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoCheckbox(
      value: properties.getBool('cupertino-checkbox-value') ?? false,
      tristate: properties.getBool('cupertino-checkbox-tristate') ?? false,
      onChanged: properties.getVoidCallback(
        'cupertino-checkbox-on-changed',
        runtime,
      ),
      activeColor: properties.getValue('cupertino-checkbox-active-color'),
      checkColor: properties.getValue('cupertino-checkbox-check-color'),
      focusColor: properties.getValue('cupertino-checkbox-focus-color'),
    );
    return IrNativeValue(Value(widget));
  });
}
