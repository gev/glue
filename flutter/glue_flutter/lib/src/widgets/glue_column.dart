import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

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

/// Glue Column widget - Flutter implementation of vertical layout
class GlueColumn extends GlueWidget {
  GlueColumn({super.properties, super.key});

  @override
  Widget build(BuildContext context) {
    final children = _parseChildren(properties['children']);
    final mainAxis =
        extractEnumValue<MainAxisAlignment>(properties['main-axis-align']) ??
        MainAxisAlignment.start;
    final crossAxis =
        extractEnumValue<CrossAxisAlignment>(properties['cross-axis-align']) ??
        CrossAxisAlignment.start;

    return Column(
      children: children,
      mainAxisAlignment: mainAxis,
      crossAxisAlignment: crossAxis,
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
