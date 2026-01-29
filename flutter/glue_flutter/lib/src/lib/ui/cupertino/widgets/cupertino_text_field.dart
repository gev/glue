import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTextField widget function
/// Creates Flutter CupertinoTextField from Glue expressions
/// Expects keyword arguments: :controller, :placeholder, :on-changed, etc.
final Ir cupertinoTextField = IrNativeFunc(cupertinoTextFieldImpl);

/// CupertinoTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTextField(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoTextField(WidgetProperties.empty()),
};

/// Create CupertinoTextField widget from properties object
Eval<Ir> _createCupertinoTextField(WidgetProperties properties) {
  final textFieldWidget = CupertinoTextField(
    key: GlobalKey(),
    controller: properties.getValue('controller'),
    placeholder: properties.getString('placeholder'),
    onChanged: properties.getValue('on-changed'),
    focusNode: properties.getValue('focus-node'),
    autofocus: properties.getBool('autofocus') ?? false,
  );
  return Eval.pure(IrNativeValue(Value(textFieldWidget)));
}
