import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

final Ir borderRoundedRectangle = IrNativeFunc(_roundedRectangleImpl);

Eval<Ir> _roundedRectangleImpl(Ir props) =>
    _createRoundedRectangle(WidgetProperties(props));

Eval<Ir> _createRoundedRectangle(WidgetProperties properties) {
  final border = RoundedRectangleBorder(
    borderRadius:
        properties.getValue<BorderRadiusGeometry>('border-radius') ??
        BorderRadius.zero,
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderCircle = IrNativeFunc(_circleImpl);

Eval<Ir> _circleImpl(Ir props) => _createCircle(WidgetProperties(props));

Eval<Ir> _createCircle(WidgetProperties properties) {
  final border = CircleBorder(
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderStadium = IrNativeFunc(_stadiumImpl);

Eval<Ir> _stadiumImpl(Ir props) => _createStadium(WidgetProperties(props));

Eval<Ir> _createStadium(WidgetProperties properties) {
  final border = StadiumBorder(
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderBeveledRectangle = IrNativeFunc(_beveledRectangleImpl);

Eval<Ir> _beveledRectangleImpl(Ir props) =>
    _createBeveledRectangle(WidgetProperties(props));

Eval<Ir> _createBeveledRectangle(WidgetProperties properties) {
  final border = BeveledRectangleBorder(
    borderRadius:
        properties.getValue<BorderRadiusGeometry>('border-radius') ??
        BorderRadius.zero,
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}
