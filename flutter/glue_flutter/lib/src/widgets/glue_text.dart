import 'package:flutter/material.dart';
import 'glue_widget.dart';

/// A text widget that can be created from Glue code expressions.
///
/// Example Glue usage:
/// ```clojure
/// (text "Hello World")
/// (text "Red Text" :color "red" :size 20)
/// ```
class GlueText extends GlueWidget {
  final String text;
  final Color? color;
  final double? fontSize;

  const GlueText({super.key, required this.text, this.color, this.fontSize});

  @override
  Widget build(BuildContext context) {
    return Text(
      text,
      style: TextStyle(color: color, fontSize: fontSize),
    );
  }
}
