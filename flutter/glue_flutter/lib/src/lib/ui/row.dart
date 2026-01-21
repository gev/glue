import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) {
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

  final rowWidget = Row(
    children: children,
    mainAxisAlignment: mainAxis,
    crossAxisAlignment: crossAxis,
  );
  return Eval.pure(IrNativeValue(HostValue(rowWidget)));
}
