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

/// Extract MainAxisAlignment from Glue IR value
MainAxisAlignment? _extractMainAxisAlignment(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: MainAxisAlignment alignment)) =>
    alignment,
  _ => null,
};

/// Extract CrossAxisAlignment from Glue IR value
CrossAxisAlignment? _extractCrossAxisAlignment(dynamic value) =>
    switch (value) {
      IrNativeValue(value: HostValue(value: CrossAxisAlignment alignment)) =>
        alignment,
      _ => null,
    };
