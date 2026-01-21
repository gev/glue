import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_padding.dart';

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

    final paddingWidget = GluePadding(
      child,
      properties: props.properties.unlock,
    );
    return Eval.pure(IrNativeValue(HostValue(paddingWidget)));
  };
}
