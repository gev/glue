import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoCheckbox widget function
/// Creates Flutter CupertinoCheckbox from Glue expressions
/// Expects keyword arguments: :value, :on-changed, etc.
final Ir cupertinoCheckbox = IrNativeFunc(cupertinoCheckboxImpl);

/// CupertinoCheckbox implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoCheckboxImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoCheckbox(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoCheckbox(CupertinoProperties.empty()),
};

/// Create CupertinoCheckbox widget from properties object
Eval<Ir> _createCupertinoCheckbox(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoCheckbox(
      value: properties.cupertinoCheckboxValue,
      tristate: properties.cupertinoCheckboxTristate,
      onChanged: properties.cupertinoCheckboxOnChanged,
      activeColor: properties.cupertinoCheckboxActiveColor,
      inactiveColor: properties.cupertinoCheckboxInactiveColor,
      checkColor: properties.cupertinoCheckboxCheckColor,
      focusColor: properties.cupertinoCheckboxFocusColor,
    );
    return IrNativeValue(Value(widget));
  });
}
