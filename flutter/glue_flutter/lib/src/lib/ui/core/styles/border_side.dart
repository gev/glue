import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BorderSide binder for Glue
final Ir borderSide = IrNativeFunc(borderSideImpl);

Eval<Ir> borderSideImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBorderSide(
    WidgetProperties(properties.unlock),
  ),
  _ => Eval.pure(IrNativeValue(Value(const BorderSide()))),
};

Eval<Ir> _createBorderSide(WidgetProperties properties) {
  final side = BorderSide(
    color: properties.getValue<Color>('color') ?? const Color(0xFF000000),
    width: properties.getDouble('width') ?? 1.0,
    style: properties.getValue<BorderStyle>('style') ?? BorderStyle.solid,
    strokeAlign:
        properties.getDouble('stroke-align') ?? BorderSide.strokeAlignInside,
  );
  return Eval.pure(IrNativeValue(Value(side)));
}
