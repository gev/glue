import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Placeholder widget function
/// Creates Flutter Placeholder from Glue (placeholder props) expressions
final Ir placeholder = IrNativeFunc(placeholderImpl);

/// Placeholder implementation - takes properties object
Eval<Ir> placeholderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPlaceholder(
    Properties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Placeholder widget from properties
Eval<Ir> _createPlaceholder(Properties properties) {
  final placeholderWidget = Placeholder(
    color: properties.color ?? const Color(0xFF455A64),
    strokeWidth: properties.size ?? 2.0,
    fallbackWidth: properties.width ?? 400.0,
    fallbackHeight: properties.height ?? 400.0,
  );
  return Eval.pure(IrNativeValue(Value(placeholderWidget)));
}
