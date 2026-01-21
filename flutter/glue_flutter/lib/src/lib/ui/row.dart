import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir arg) => switch (arg) {
  IrObject(:final properties) => _createRow(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Row widget from properties
Eval<Ir> _createRow(IMap<String, Ir> properties) {
  // Extract properties using lazy wrapper
  final props = Properties(properties.unlock);
  final rowWidget = Row(
    children: props.children,
    mainAxisAlignment: props.mainAlign,
    crossAxisAlignment: props.crossAlign,
  );
  return Eval.pure(IrNativeValue(HostValue(rowWidget)));
}
