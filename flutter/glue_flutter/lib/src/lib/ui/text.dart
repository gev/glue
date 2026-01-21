import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text content props) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes content string
Eval<Ir> textImpl(Ir content) {
  return Eval.pure(IrNativeFunc(textWithContent(content)));
}

/// Text with content - takes properties object
Eval<Ir> Function(Ir) textWithContent(Ir content) =>
    (Ir props) => switch ((content, props)) {
      (IrString(:final value), IrObject(:final properties)) => _createText(
        value,
        Properties(properties.unlock),
      ),
      _ => throwError(wrongArgumentType(['string', 'object'])),
    };

/// Create Text widget from content and properties
Eval<Ir> _createText(String content, Properties properties) {
  final textWidget = Text(
    content,
    style: TextStyle(
      color: properties.color,
      fontSize: properties.size,
      fontWeight: properties.weight,
    ),
    textAlign: properties.align,
  );
  return Eval.pure(IrNativeValue(HostValue(textWidget)));
}
