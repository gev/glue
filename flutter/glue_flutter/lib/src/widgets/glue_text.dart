import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';
import '../utils/color_parser.dart';

/// Helper function to extract enum value from HostValue only (no parsing)
T? extractEnumValue<T>(Ir? ir) {
  if (ir == null) return null;

  // Only accept direct enum objects - no string parsing
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    if (hostValue.value is T) {
      return hostValue.value as T;
    }
  }

  return null; // No fallback parsing
}

/// Extract color value - handles both Color objects and hex string parsing
Color? extractColorValue(Ir? ir) {
  if (ir == null) return null;

  // If it's a direct Color object, use it
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    if (hostValue.value is Color) {
      return hostValue.value as Color;
    }
  }

  // Otherwise, parse as string (hex colors, named colors)
  return parseColor(ir);
}

/// Glue Text widget - Flutter implementation of text display
class GlueText extends GlueWidget {
  final String text;

  GlueText(this.text, {super.properties, super.key});

  @override
  Widget build(BuildContext context) {
    final color = extractColorValue(properties['color']);
    final size = properties['size'];
    final weight = extractEnumValue<FontWeight>(properties['weight']);
    final align = extractEnumValue<TextAlign>(properties['align']);

    double? fontSize;
    if (size is IrInteger) {
      fontSize = size.value.toDouble();
    } else if (size is IrFloat) {
      fontSize = size.value;
    }

    return Text(
      text,
      style: TextStyle(color: color, fontSize: fontSize, fontWeight: weight),
      textAlign: align,
    );
  }
}
