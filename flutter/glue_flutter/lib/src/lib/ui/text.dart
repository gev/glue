import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

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
    final color = _extractColor(properties['color']);
    final size = _extractDouble(properties['size']);
    final weight = _extractFontWeight(properties['weight']);
    final align = _extractTextAlign(properties['align']);

    final textWidget = Text(
      content.value,
      style: TextStyle(color: color, fontSize: size, fontWeight: weight),
      textAlign: align,
    );
    return Eval.pure(IrNativeValue(HostValue(textWidget)));
  };
}

/// Extract color from Glue IR value
Color? _extractColor(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: Color color)) => color,
  _ => null, // TODO: Handle hex string parsing
};

/// Extract double from Glue IR value
double? _extractDouble(dynamic value) => switch (value) {
  IrInteger(:final value) => value.toDouble(),
  IrFloat(:final value) => value,
  _ => null,
};

/// Extract FontWeight from Glue IR value
FontWeight? _extractFontWeight(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: FontWeight weight)) => weight,
  _ => null,
};

/// Extract TextAlign from Glue IR value
TextAlign? _extractTextAlign(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: TextAlign align)) => align,
  _ => null,
};
