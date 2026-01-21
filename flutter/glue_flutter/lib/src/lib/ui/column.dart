import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

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
  final children = extractChildren(properties['children']) ?? [];
  final mainAxis =
      extractMainAxisAlignment(properties['main-axis-align']) ??
      MainAxisAlignment.start;
  final crossAxis =
      extractCrossAxisAlignment(properties['cross-axis-align']) ??
      CrossAxisAlignment.start;

  final columnWidget = Column(
    children: children,
    mainAxisAlignment: mainAxis,
    crossAxisAlignment: crossAxis,
  );
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}
