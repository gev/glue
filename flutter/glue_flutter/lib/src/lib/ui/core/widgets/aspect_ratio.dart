import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

/// AspectRatio widget function
final Ir aspectRatio = IrNativeFunc(_aspectRatioImpl);

Eval<Ir> _aspectRatioImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAspectRatio(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createAspectRatio(WidgetProperties properties) {
  final widget = AspectRatio(
    key: properties.key,
    aspectRatio: properties.getDouble('aspect-ratio') ?? 1.0,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
