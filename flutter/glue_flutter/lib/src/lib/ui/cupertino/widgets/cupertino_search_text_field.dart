import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSearchTextField widget function
/// Creates Flutter CupertinoSearchTextField from Glue expressions
/// Expects keyword arguments: :controller, :on-changed, :on-submitted, :hint-text, etc.
final Ir cupertinoSearchTextField = IrNativeFunc(cupertinoSearchTextFieldImpl);

/// CupertinoSearchTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSearchTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSearchTextField(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSearchTextField(WidgetProperties.empty()),
};

/// Create CupertinoSearchTextField widget from properties object
Eval<Ir> _createCupertinoSearchTextField(WidgetProperties properties) {
  final widget = CupertinoSearchTextField(
    controller: properties.getValue('controller'),
    onChanged: properties.getValue('on-changed'),
    onSubmitted: properties.getValue('on-submitted'),
    placeholder: properties.getString('placeholder'),
    placeholderStyle: properties.getValue('placeholder-style'),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
