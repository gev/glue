import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/edge_insets_parser.dart';

/// Padding widget function
/// Creates Flutter Padding from Glue (padding child props) expressions
final Ir padding = IrNativeFunc(paddingImpl);

/// Padding implementation - takes child, then properties
Eval<Ir> paddingImpl(Ir child) {
  return Eval.pure(IrNativeFunc(paddingWithChild(child)));
}

/// Padding with child - takes properties object
Eval<Ir> Function(Ir) paddingWithChild(Ir child) {
  return (Ir props) {
    if (child is! IrNativeValue) {
      return throwError(wrongArgumentType(['widget']));
    }
    if (props is! IrObject) {
      return throwError(wrongArgumentType(['object']));
    }

    // Extract the child widget from IrNativeValue
    final hostValue = child.value;
    if (hostValue.value is! Widget) {
      return throwError(wrongArgumentType(['widget']));
    }
    final childWidget = hostValue.value as Widget;

    // Extract padding from properties
    final properties = props.properties.unlock as Map<String, dynamic>;
    final padding = properties['padding'] != null
        ? parseEdgeInsets(properties['padding']!)
        : EdgeInsets.zero;

    final paddingWidget = Padding(
      padding: padding ?? EdgeInsets.zero,
      child: childWidget,
    );
    return Eval.pure(IrNativeValue(HostValue(paddingWidget)));
  };
}
