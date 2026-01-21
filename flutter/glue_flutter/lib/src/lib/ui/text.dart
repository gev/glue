import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text content props) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes content string
Eval<Ir> textImpl(Ir content) {
  return Eval.pure(IrNativeFunc(textWithContent(content)));
}

/// Text with content - takes properties object
Eval<Ir> Function(Ir) textWithContent(Ir content) {
  return (Ir props) {
    if (content is! IrString) {
      return throwError(wrongArgumentType(['string']));
    }
    if (props is! IrObject) {
      return throwError(wrongArgumentType(['object']));
    }

    // Extract values from Glue IR properties
    final properties = props.properties.unlock as Map<String, dynamic>;
    final color = extractColor(properties['color']);
    final size = extractDouble(properties['size']);
    final weight = extractFontWeight(properties['weight']);
    final align = extractTextAlign(properties['align']);

    final textWidget = Text(
      content.value,
      style: TextStyle(color: color, fontSize: size, fontWeight: weight),
      textAlign: align,
    );
    return Eval.pure(IrNativeValue(HostValue(textWidget)));
  };
}
