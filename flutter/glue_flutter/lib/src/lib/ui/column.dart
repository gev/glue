import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  // Extract values from Glue IR properties
  final properties = props.properties.unlock as Map<String, dynamic>;
  final children = _extractChildren(properties['children']) ?? [];
  final mainAxis =
      _extractMainAxisAlignment(properties['main-axis-align']) ??
      MainAxisAlignment.start;
  final crossAxis =
      _extractCrossAxisAlignment(properties['cross-axis-align']) ??
      CrossAxisAlignment.start;

  final columnWidget = Column(
    children: children,
    mainAxisAlignment: mainAxis,
    crossAxisAlignment: crossAxis,
  );
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}

/// Extract children list from Glue IR value
List<Widget>? _extractChildren(dynamic value) {
  if (value == null) return null;
  if (value is! List) return null;

  return value.map((child) {
    if (child is IrNativeValue) {
      final hostValue = child.value;
      if (hostValue.value is Widget) {
        return hostValue.value as Widget;
      }
    }
    return const SizedBox.shrink(); // Invalid child
  }).toList();
}

/// Extract MainAxisAlignment from Glue IR value
MainAxisAlignment? _extractMainAxisAlignment(dynamic value) {
  if (value == null) return null;

  // Only accept direct enum objects - no string parsing
  if (value is IrNativeValue) {
    final hostValue = value.value;
    if (hostValue.value is MainAxisAlignment) {
      return hostValue.value as MainAxisAlignment;
    }
  }

  return null;
}

/// Extract CrossAxisAlignment from Glue IR value
CrossAxisAlignment? _extractCrossAxisAlignment(dynamic value) {
  if (value == null) return null;

  // Only accept direct enum objects - no string parsing
  if (value is IrNativeValue) {
    final hostValue = value.value;
    if (hostValue.value is CrossAxisAlignment) {
      return hostValue.value as CrossAxisAlignment;
    }
  }

  return null;
}
