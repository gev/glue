import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Row widget - Flutter implementation of horizontal layout
class GlueRow extends GlueWidget {
  final List<Widget> children;
  final MainAxisAlignment mainAxisAlignment;
  final CrossAxisAlignment crossAxisAlignment;

  const GlueRow({
    required this.children,
    this.mainAxisAlignment = MainAxisAlignment.start,
    this.crossAxisAlignment = CrossAxisAlignment.start,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return Row(
      children: children,
      mainAxisAlignment: mainAxisAlignment,
      crossAxisAlignment: crossAxisAlignment,
    );
  }
}
