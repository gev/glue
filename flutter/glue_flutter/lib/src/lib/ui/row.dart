import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) => switch (props) {
  IrObject() => () {
    // Extract properties using lazy wrapper
    final properties = Properties(props.properties.unlock);

    final rowWidget = Row(
      children: properties.children,
      mainAxisAlignment: properties.mainAlign,
      crossAxisAlignment: properties.crossAlign,
    );
    return Eval.pure(IrNativeValue(HostValue(rowWidget)));
  }(),
  _ => throwError(wrongArgumentType(['object'])),
};
