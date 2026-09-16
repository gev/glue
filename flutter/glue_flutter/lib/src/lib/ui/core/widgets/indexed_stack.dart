import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// IndexedStack widget function
/// Creates Flutter IndexedStack from Glue expressions
final Ir indexedStack = IrNativeFunc(indexedStackImpl);

/// IndexedStack implementation - takes properties object
Eval<Ir> indexedStackImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createIndexedStack(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create IndexedStack widget from properties
Eval<Ir> _createIndexedStack(WidgetProperties properties) {
  final indexedStackWidget = IndexedStack(
    key: properties.key,
    index: properties.getInt('index') ?? 0,
    alignment:
        properties.getValue<Alignment>('alignment') ??
        AlignmentDirectional.topStart,
    textDirection: properties.getValue<TextDirection>('text-direction'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    sizing: properties.getValue<StackFit>('sizing') ?? StackFit.loose,
    children: properties.children,
  );
  return Eval.pure(IrNativeValue(Value(indexedStackWidget)));
}
