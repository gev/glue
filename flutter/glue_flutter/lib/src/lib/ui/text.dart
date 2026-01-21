import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_text.dart';

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

    final textWidget = GlueText(
      content.value,
      color: color,
      fontSize: size,
      fontWeight: weight,
      textAlign: align,
    );
    return Eval.pure(IrNativeValue(HostValue(textWidget)));
  };
}

/// Extract color from Glue IR value
Color? _extractColor(dynamic value) {
  if (value == null) return null;

  // If it's a direct Color object from enum
  if (value is IrNativeValue) {
    final hostValue = value.value;
    if (hostValue.value is Color) {
      return hostValue.value as Color;
    }
  }

  // TODO: Handle hex string parsing
  return null;
}

/// Extract double from Glue IR value
double? _extractDouble(dynamic value) {
  if (value == null) return null;

  if (value is IrInteger) return value.value.toDouble();
  if (value is IrFloat) return value.value;

  return null;
}

/// Extract FontWeight from Glue IR value
FontWeight? _extractFontWeight(dynamic value) {
  if (value == null) return null;

  // Only accept direct enum objects - no string parsing
  if (value is IrNativeValue) {
    final hostValue = value.value;
    if (hostValue.value is FontWeight) {
      return hostValue.value as FontWeight;
    }
  }

  return null;
}

/// Extract TextAlign from Glue IR value
TextAlign? _extractTextAlign(dynamic value) {
  if (value == null) return null;

  // Only accept direct enum objects - no string parsing
  if (value is IrNativeValue) {
    final hostValue = value.value;
    if (hostValue.value is TextAlign) {
      return hostValue.value as TextAlign;
    }
  }

  return null;
}
