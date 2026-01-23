import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text :content "text" ...) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes properties object with :content, :color, etc.
Eval<Ir> textImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createText(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Text widget from properties (extracts :content, :color, etc.)
Eval<Ir> _createText(Properties properties) {
  final content = properties.content ?? '';
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
