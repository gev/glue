import 'package:flutter/cupertino.dart';
import 'package:glue/error.dart';
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
  final onChanged = properties.getCallback<bool>('on-changed');
  if (onChanged == null) {
    return throwError(wrongArgumentType(['on-changed callback required']));
  }
  return getRuntime().map((runtime) {
    final widget = CupertinoCheckbox(
      key: properties.key,
      value: properties.getBool('value') ?? false,
      tristate: properties.getBool('tristate') ?? false,
      onChanged: onChanged(runtime),
      activeColor: properties.getColor('active-color'),
      checkColor: properties.getColor('check-color'),
      focusColor: properties.getColor('focus-color'),
      autofocus: properties.getBool('autofocus') ?? false,
    );
    return IrNativeValue(Value(widget));
  });
}
