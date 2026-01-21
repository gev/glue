import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval.dart';
import 'glue_widget.dart';
import '../utils/color_parser.dart';

/// Helper function to extract enum value from HostValue or parse string
T? extractEnumOrParse<T>(Ir? ir, T? Function(Ir) parseFunction) {
  if (ir == null) return null;

  // If it's already a HostValue with the enum, extract it directly
  if (ir is IrNativeValue && ir.value is HostValue) {
    final hostValue = ir.value as HostValue;
    if (hostValue.value is T) {
      return hostValue.value as T;
    }
  }

  // Otherwise, parse as string for backward compatibility
  return parseFunction(ir);
}

/// Parse font weight from string or integer
FontWeight? _parseFontWeight(Ir ir) {
  return switch (ir) {
    IrString(value: final weightStr) => _parseFontWeightString(weightStr),
    IrInteger(value: final weightInt) => _parseFontWeightInt(weightInt),
    _ => null,
  };
}

FontWeight? _parseFontWeightString(String weight) {
  return switch (weight.toLowerCase()) {
    'normal' => FontWeight.normal,
    'bold' => FontWeight.bold,
    'w100' => FontWeight.w100,
    'w200' => FontWeight.w200,
    'w300' => FontWeight.w300,
    'w400' => FontWeight.w400,
    'w500' => FontWeight.w500,
    'w600' => FontWeight.w600,
    'w700' => FontWeight.w700,
    'w800' => FontWeight.w800,
    'w900' => FontWeight.w900,
    _ => null,
  };
}

FontWeight? _parseFontWeightInt(int weight) {
  return switch (weight) {
    100 => FontWeight.w100,
    200 => FontWeight.w200,
    300 => FontWeight.w300,
    400 => FontWeight.w400,
    500 => FontWeight.w500,
    600 => FontWeight.w600,
    700 => FontWeight.w700,
    800 => FontWeight.w800,
    900 => FontWeight.w900,
    _ => null,
  };
}

/// Parse text alignment from string
TextAlign? _parseTextAlign(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => switch (alignStr.toLowerCase()) {
      'left' => TextAlign.left,
      'right' => TextAlign.right,
      'center' => TextAlign.center,
      'justify' => TextAlign.justify,
      'start' => TextAlign.start,
      'end' => TextAlign.end,
      _ => null,
    },
    _ => null,
  };
}

/// Glue Text widget - Flutter implementation of text display
class GlueText extends GlueWidget {
  final String text;
  final Map<String, dynamic> properties;

  const GlueText(this.text, this.properties);

  @override
  Widget build(BuildContext context) {
    final color = properties['color'] != null
        ? parseColor(properties['color']!)
        : null;
    final size = properties['size'];
    final weight = extractEnumOrParse<FontWeight>(
      properties['weight'],
      (ir) => _parseFontWeight(ir),
    );
    final align = extractEnumOrParse<TextAlign>(
      properties['align'],
      (ir) => _parseTextAlign(ir),
    );

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
