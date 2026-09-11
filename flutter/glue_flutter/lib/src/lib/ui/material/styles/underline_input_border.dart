import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// UnderlineInputBorder function
/// Creates Flutter UnderlineInputBorder from Glue expressions
final Ir underlineInputBorder = IrNativeFunc(underlineInputBorderImpl);

/// UnderlineInputBorder implementation - takes properties object
Eval<Ir> underlineInputBorderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createUnderlineInputBorder(
    WidgetProperties(properties.unlock),
  ),
  _ => _createUnderlineInputBorder(WidgetProperties.empty()),
};

/// Create UnderlineInputBorder from properties
Eval<Ir> _createUnderlineInputBorder(WidgetProperties properties) {
  final border = UnderlineInputBorder(
    borderSide:
        properties.getValue<BorderSide>('border-side') ?? const BorderSide(),
    borderRadius:
        properties.getValue<BorderRadius>('border-radius') ??
        const BorderRadius.only(
          topLeft: Radius.circular(4.0),
          topRight: Radius.circular(4.0),
        ),
  );
  return Eval.pure(IrNativeValue(Value(border)));
}
