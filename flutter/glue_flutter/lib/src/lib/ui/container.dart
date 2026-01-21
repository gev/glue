import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

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
  final children = extractChildren(properties['children']) ?? [];
  final direction = extractAxis(properties['direction']) ?? Axis.vertical;
  final spacing = extractDouble(properties['spacing']);

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

/// Build children with spacing
List<Widget> _buildChildrenWithSpacing(List<Widget> children, double? gap) =>
    gap == null || gap == 0 || children.isEmpty
          ? children
          : children
                .expand((child) => [child, SizedBox(width: gap, height: gap)])
                .toList()
      ..removeLast();
