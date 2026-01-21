import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_container.dart';

/// Container widget function
/// Creates Flutter Column/Row from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object (children in props)
Eval<Ir> containerImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  // Extract values from Glue IR properties
  final properties = props.properties.unlock as Map<String, dynamic>;
  final children = _extractChildren(properties['children']) ?? [];
  final direction = _extractAxis(properties['direction']) ?? Axis.vertical;
  final spacing = _extractDouble(properties['spacing']);

  final containerWidget = GlueContainer(
    children: children,
    direction: direction,
    spacing: spacing,
  );
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
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

/// Extract Axis from Glue IR value
Axis? _extractAxis(dynamic value) {
  if (value == null) return null;
  if (value is IrString) {
    return switch (value.value) {
      'horizontal' => Axis.horizontal,
      'vertical' => Axis.vertical,
      _ => null,
    };
  }
  return null;
}

/// Extract double from Glue IR value
double? _extractDouble(dynamic value) {
  if (value == null) return null;
  if (value is IrInteger) return value.value.toDouble();
  if (value is IrFloat) return value.value;
  if (value is double) return value;
  if (value is int) return value.toDouble();
  return null;
}
