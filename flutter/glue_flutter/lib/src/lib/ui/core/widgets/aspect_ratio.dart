import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

/// AspectRatio widget function
final Ir aspectRatio = IrNativeFunc(_aspectRatioImpl);

Eval<Ir> _aspectRatioImpl(Ir props) =>
    _createAspectRatio(WidgetProperties(props));

Eval<Ir> _createAspectRatio(WidgetProperties properties) {
  final widget = AspectRatio(
    key: properties.key,
    aspectRatio: properties.getDouble('aspect-ratio') ?? 1.0,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
