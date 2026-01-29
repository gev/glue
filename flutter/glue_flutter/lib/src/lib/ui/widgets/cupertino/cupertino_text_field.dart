import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoTextField widget function
/// Creates Flutter CupertinoTextField from Glue expressions
/// Expects keyword arguments: :controller, :placeholder, :on-changed, etc.
final Ir cupertinoTextField = IrNativeFunc(cupertinoTextFieldImpl);

/// CupertinoTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTextField(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoTextField(CupertinoProperties.empty()),
};

/// Create CupertinoTextField widget from properties object
Eval<Ir> _createCupertinoTextField(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final onChangedCallback = properties.onTextChanged != null
        ? (String value) => properties.onTextChanged!(value)
        : null;

    final textFieldWidget = CupertinoTextField(
      key: GlobalKey(),
      controller: properties.textEditingController,
      placeholder: properties.cupertinoTextFieldPlaceholder,
      onChanged: onChangedCallback,
      focusNode: properties.dropdownFocusNode,
      autofocus: properties.dropdownAutofocus,
    );
    return IrNativeValue(Value(textFieldWidget));
  });
}
