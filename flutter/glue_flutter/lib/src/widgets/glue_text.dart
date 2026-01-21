import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';
import '../utils/color_parser.dart';
import '../utils/font_weight_parser.dart';
import '../utils/text_align_parser.dart';

/// Glue Text widget - Flutter implementation of text display
class GlueText extends GlueWidget {
  final String text;
  final IMap<String, dynamic> properties;

  const GlueText(this.text, this.properties);

  @override
  Widget build(BuildContext context) {
    final color = properties['color'] != null
        ? parseColor(properties['color']!)
        : null;
    final size = properties['size'];
    final weight = properties['weight'] != null
        ? parseFontWeight(properties['weight']!)
        : null;
    final align = properties['align'] != null
        ? parseTextAlign(properties['align']!)
        : null;

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
