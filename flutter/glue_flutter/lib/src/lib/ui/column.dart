import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  // Extract properties using lazy wrapper
  final properties = Properties(props.properties.unlock);

  final columnWidget = Column(
    children: properties.children ?? [],
    mainAxisAlignment: properties.mainAlign ?? MainAxisAlignment.start,
    crossAxisAlignment: properties.crossAlign ?? CrossAxisAlignment.start,
  );
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}
