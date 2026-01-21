import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Container widget - Flutter implementation of layout container
class GlueContainer extends GlueWidget {
  final List<Widget> children;
  final Axis direction;
  final double? spacing;

  const GlueContainer({
    required this.children,
    this.direction = Axis.vertical,
    this.spacing,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    final spacedChildren = _buildChildrenWithSpacing(children, spacing);

    return switch (direction) {
      Axis.horizontal => Row(
        children: spacedChildren,
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
      ),
      Axis.vertical => Column(
        children: spacedChildren,
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
      ),
    };
  }

  List<Widget> _buildChildrenWithSpacing(List<Widget> children, double? gap) {
    if (gap == null || gap == 0 || children.isEmpty) {
      return children;
    }

    final spacedChildren = <Widget>[];
    for (var i = 0; i < children.length; i++) {
      spacedChildren.add(children[i]);
      if (i < children.length - 1) {
        if (direction == Axis.horizontal) {
          spacedChildren.add(SizedBox(width: gap));
        } else {
          spacedChildren.add(SizedBox(height: gap));
        }
      }
    }
    return spacedChildren;
  }
}
