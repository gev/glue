import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

final Ir borderRoundedRectangle = IrNativeFunc(_roundedRectangleImpl);

Eval<Ir> _roundedRectangleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRoundedRectangle(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

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

Eval<Ir> _circleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCircle(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createCircle(WidgetProperties properties) {
  final border = CircleBorder(
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderStadium = IrNativeFunc(_stadiumImpl);

Eval<Ir> _stadiumImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createStadium(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createStadium(WidgetProperties properties) {
  final border = StadiumBorder(
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderBeveledRectangle = IrNativeFunc(_beveledRectangleImpl);

Eval<Ir> _beveledRectangleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBeveledRectangle(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createBeveledRectangle(WidgetProperties properties) {
  final border = BeveledRectangleBorder(
    borderRadius:
        properties.getValue<BorderRadiusGeometry>('border-radius') ??
        BorderRadius.zero,
    side: properties.getValue<BorderSide>('side') ?? BorderSide.none,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}
