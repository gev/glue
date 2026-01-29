import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoSearchTextField widget function
/// Creates Flutter CupertinoSearchTextField from Glue expressions
/// Expects keyword arguments: :controller, :on-changed, :on-submitted, :hint-text, etc.
final Ir cupertinoSearchTextField = IrNativeFunc(cupertinoSearchTextFieldImpl);

/// CupertinoSearchTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSearchTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSearchTextField(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoSearchTextField(CupertinoProperties.empty()),
};

/// Create CupertinoSearchTextField widget from properties object
Eval<Ir> _createCupertinoSearchTextField(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoSearchTextField(
      controller: properties.cupertinoSearchTextFieldController,
      onChanged: properties.cupertinoSearchTextFieldOnChanged,
      onSubmitted: properties.cupertinoSearchTextFieldOnSubmitted,
      style: properties.cupertinoSearchTextFieldStyle,
      placeholder: properties.cupertinoSearchTextFieldHintText,
    );
    return IrNativeValue(Value(widget));
  });
}
