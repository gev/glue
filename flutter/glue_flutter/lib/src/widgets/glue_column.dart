import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Column widget - Flutter implementation of vertical layout
class GlueColumn extends GlueWidget {
  final List<Widget> children;
  final MainAxisAlignment mainAxisAlignment;
  final CrossAxisAlignment crossAxisAlignment;

  const GlueColumn({
    required this.children,
    this.mainAxisAlignment = MainAxisAlignment.start,
    this.crossAxisAlignment = CrossAxisAlignment.start,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return Column(
      children: children,
      mainAxisAlignment: mainAxisAlignment,
      crossAxisAlignment: crossAxisAlignment,
    );
  }
}
