import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

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

  final containerWidget = direction == Axis.horizontal
      ? Row(
          children: _buildChildrenWithSpacing(children, spacing),
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
        )
      : Column(
          children: _buildChildrenWithSpacing(children, spacing),
          mainAxisAlignment: MainAxisAlignment.start,
          crossAxisAlignment: CrossAxisAlignment.start,
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

/// Build children with spacing
List<Widget> _buildChildrenWithSpacing(List<Widget> children, double? gap) {
  if (gap == null || gap == 0 || children.isEmpty) {
    return children;
  }

  final spacedChildren = <Widget>[];
  for (var i = 0; i < children.length; i++) {
    spacedChildren.add(children[i]);
    if (i < children.length - 1) {
      spacedChildren.add(SizedBox(width: gap, height: gap));
    }
  }
  return spacedChildren;
}
