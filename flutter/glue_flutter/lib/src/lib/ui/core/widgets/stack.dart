import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Stack widget function
/// Creates Flutter Stack from Glue (stack props) expressions
final Ir stack = IrNativeFunc(stackImpl);

/// Stack implementation - takes properties object
Eval<Ir> stackImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createStack(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Stack widget from properties
Eval<Ir> _createStack(WidgetProperties properties) {
  final stackWidget = Stack(
    key: properties.key,
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ??
        AlignmentDirectional.topStart,
    textDirection: properties.getValue<TextDirection>('text-direction'),
    fit: properties.getValue<StackFit>('fit') ?? StackFit.loose,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    children: properties.children,
  );
  return Eval.pure(IrNativeValue(Value(stackWidget)));
}
