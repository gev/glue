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
List<Widget>? _extractChildren(dynamic value) => switch (value) {
  List list =>
    list
        .map(
          (child) => switch (child) {
            IrNativeValue(value: HostValue(value: Widget widget)) => widget,
            _ => const SizedBox.shrink(),
          },
        )
        .toList(),
  _ => null,
};

/// Extract Axis from Glue IR value
Axis? _extractAxis(dynamic value) => switch (value) {
  IrString(value: 'horizontal') => Axis.horizontal,
  IrString(value: 'vertical') => Axis.vertical,
  _ => null,
};

/// Extract double from Glue IR value
double? _extractDouble(dynamic value) => switch (value) {
  IrInteger(:final value) => value.toDouble(),
  IrFloat(:final value) => value,
  double d => d,
  int i => i.toDouble(),
  _ => null,
};

/// Build children with spacing
List<Widget> _buildChildrenWithSpacing(List<Widget> children, double? gap) =>
    gap == null || gap == 0 || children.isEmpty
          ? children
          : children
                .expand((child) => [child, SizedBox(width: gap, height: gap)])
                .toList()
      ..removeLast();
