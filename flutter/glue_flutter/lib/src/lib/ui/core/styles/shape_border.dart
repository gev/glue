import 'package:flutter/material.dart';
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

final Ir borderRadiusCircular = IrNativeFunc(_radiusCircularImpl);

Eval<Ir> _radiusCircularImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRadiusCircular(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createRadiusCircular(WidgetProperties properties) {
  final r = properties.getDouble('radius') ?? 0.0;
  return Eval.pure(IrNativeValue(Value(BorderRadius.circular(r))));
}

final Ir borderRadiusOnly = IrNativeFunc(_radiusOnlyImpl);

Eval<Ir> _radiusOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRadiusOnly(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createRadiusOnly(WidgetProperties properties) {
  final border = BorderRadius.only(
    topLeft: properties.getValue<Radius>('top-left') ?? Radius.zero,
    topRight: properties.getValue<Radius>('top-right') ?? Radius.zero,
    bottomLeft: properties.getValue<Radius>('bottom-left') ?? Radius.zero,
    bottomRight: properties.getValue<Radius>('bottom-right') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}

final Ir borderRadiusVal = IrNativeFunc(_radiusValImpl);

Eval<Ir> _radiusValImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRadiusVal(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> _createRadiusVal(WidgetProperties properties) {
  final r = properties.getDouble('radius') ?? 0.0;
  return Eval.pure(IrNativeValue(Value(Radius.circular(r))));
}
