import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';

/// Glue Container widget - Flutter implementation of layout container
class GlueContainer extends GlueWidget {
  const GlueContainer(super.properties);

  @override
  Widget build(BuildContext context) {
    final children = _parseChildren(properties['children']);
    final direction = properties['direction'] as String? ?? 'vertical';
    final spacing = properties['spacing'];

    double? gap;
    if (spacing is int) {
      gap = spacing.toDouble();
    } else if (spacing is double) {
      gap = spacing;
    }

    return switch (direction) {
      'horizontal' => Row(
        children: _buildChildrenWithSpacing(children, gap, isHorizontal: true),
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
      ),
      'vertical' => Column(
        children: _buildChildrenWithSpacing(children, gap, isHorizontal: false),
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
      ),
      _ => Column(
        children: _buildChildrenWithSpacing(children, gap, isHorizontal: false),
        mainAxisAlignment: MainAxisAlignment.start,
        crossAxisAlignment: CrossAxisAlignment.start,
      ),
    };
  }

  List<Widget> _parseChildren(dynamic childrenProp) {
    if (childrenProp is List) {
      return childrenProp.map((child) {
        if (child is IrNativeValue) {
          final hostValue = child.value;
          if (hostValue.value is GlueWidget) {
            return hostValue.value as GlueWidget;
          }
        }
        return const SizedBox.shrink(); // Invalid child
      }).toList();
    }
    return [];
  }

  List<Widget> _buildChildrenWithSpacing(
    List<Widget> children,
    double? gap, {
    required bool isHorizontal,
  }) {
    if (gap == null || gap == 0 || children.isEmpty) {
      return children;
    }

    final spacedChildren = <Widget>[];
    for (var i = 0; i < children.length; i++) {
      spacedChildren.add(children[i]);
      if (i < children.length - 1) {
        if (isHorizontal) {
          spacedChildren.add(SizedBox(width: gap));
        } else {
          spacedChildren.add(SizedBox(height: gap));
        }
      }
    }
    return spacedChildren;
  }
}
