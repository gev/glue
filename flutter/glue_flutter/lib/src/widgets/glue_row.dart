import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'glue_widget.dart';
import '../utils/main_axis_alignment_parser.dart';
import '../utils/cross_axis_alignment_parser.dart';

/// Glue Row widget - Flutter implementation of horizontal layout
class GlueRow extends GlueWidget {
  final Map<String, dynamic> properties;

  const GlueRow(this.properties);

  @override
  Widget build(BuildContext context) {
    final children = _parseChildren(properties['children']);
    final mainAxis = properties['main-axis-align'] != null
        ? parseMainAxisAlignment(properties['main-axis-align']!)
        : MainAxisAlignment.start;
    final crossAxis = properties['cross-axis-align'] != null
        ? parseCrossAxisAlignment(properties['cross-axis-align']!)
        : CrossAxisAlignment.start;

    return Row(
      children: children,
      mainAxisAlignment: mainAxis ?? MainAxisAlignment.start,
      crossAxisAlignment: crossAxis ?? CrossAxisAlignment.start,
    );
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
}
