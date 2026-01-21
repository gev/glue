import 'package:flutter/material.dart';
import 'glue_widget.dart';

/// A container widget that can hold multiple child widgets from Glue code expressions.
///
/// Example Glue usage:
/// ```clojure
/// (container :children [(text "Child 1") (text "Child 2")])
/// ```
class GlueContainer extends GlueWidget {
  final List<GlueWidget> children;

  const GlueContainer({super.key, required this.children});

  @override
  Widget build(BuildContext context) {
    return Column(children: children);
  }
}
