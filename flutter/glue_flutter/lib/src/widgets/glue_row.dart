import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval.dart';
import 'glue_widget.dart';

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

/// Parse main axis alignment from string
MainAxisAlignment? _parseMainAxisAlignment(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => switch (alignStr.toLowerCase()) {
      'start' => MainAxisAlignment.start,
      'end' => MainAxisAlignment.end,
      'center' => MainAxisAlignment.center,
      'spacebetween' || 'space-between' => MainAxisAlignment.spaceBetween,
      'spacearound' || 'space-around' => MainAxisAlignment.spaceAround,
      'spaceevenly' || 'space-evenly' => MainAxisAlignment.spaceEvenly,
      _ => null,
    },
    _ => null,
  };
}

/// Parse cross axis alignment from string
CrossAxisAlignment? _parseCrossAxisAlignment(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => switch (alignStr.toLowerCase()) {
      'start' => CrossAxisAlignment.start,
      'end' => CrossAxisAlignment.end,
      'center' => CrossAxisAlignment.center,
      'stretch' => CrossAxisAlignment.stretch,
      'baseline' => CrossAxisAlignment.baseline,
      _ => null,
    },
    _ => null,
  };
}

/// Glue Row widget - Flutter implementation of horizontal layout
class GlueRow extends GlueWidget {
  final Map<String, dynamic> properties;

  const GlueRow(this.properties);

  @override
  Widget build(BuildContext context) {
    final children = _parseChildren(properties['children']);
    final mainAxis =
        extractEnumOrParse<MainAxisAlignment>(
          properties['main-axis-align'],
          (ir) => _parseMainAxisAlignment(ir),
        ) ??
        MainAxisAlignment.start;
    final crossAxis =
        extractEnumOrParse<CrossAxisAlignment>(
          properties['cross-axis-align'],
          (ir) => _parseCrossAxisAlignment(ir),
        ) ??
        CrossAxisAlignment.start;

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
