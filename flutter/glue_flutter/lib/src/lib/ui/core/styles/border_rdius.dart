import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

final borderRadiusZero = IrNativeValue(Value(BorderRadius.zero));

final borderRadiusAll = IrNativeFunc(borderRadiusAllImpl);

Eval<Ir> borderRadiusAllImpl(Ir value) {
  final radius = extractNativeValue<Radius>(value);
  if (radius == null) {
    return throwError(wrongArgumentType(['radius']));
  }
  return createBorderRadiusCircular(BorderRadius.all(radius));
}

Eval<Ir> createBorderRadiusAll(BorderRadiusGeometry radius) {
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusCircular = IrNativeFunc(borderRadiusCircularImpl);

Eval<Ir> borderRadiusCircularImpl(Ir value) {
  final radius = extractDouble(value);
  if (radius == null) {
    return throwError(wrongArgumentType(['number']));
  }
  return createBorderRadiusCircular(BorderRadius.circular(radius));
}

Eval<Ir> createBorderRadiusCircular(BorderRadiusGeometry radius) {
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusDirectional = IrNativeFunc(borderRadiusVerticalImpl);

Eval<Ir> borderRadiusDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderRadiusVertical(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderRadiusDirectional(WidgetProperties properties) {
  final radius = BorderRadiusDirectional.only(
    topStart: properties.getValue<Radius>('top-start') ?? Radius.zero,
    topEnd: properties.getValue<Radius>('top-end') ?? Radius.zero,
    bottomStart: properties.getValue<Radius>('bottom-start') ?? Radius.zero,
    bottomEnd: properties.getValue<Radius>('bottom-end') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusHorizontal = IrNativeFunc(borderRadiusVerticalImpl);

Eval<Ir> borderRadiusHorizontalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderRadiusVertical(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderRadiusHorizontal(WidgetProperties properties) {
  final radius = BorderRadius.horizontal(
    left: properties.getValue<Radius>('left') ?? Radius.zero,
    right: properties.getValue<Radius>('right') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusDirectionalHorizontal = IrNativeFunc(
  borderRadiusVerticalImpl,
);

Eval<Ir> borderRadiusDirectionalHorizontalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderRadiusVertical(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderRadiusDirectionalHorizontal(WidgetProperties properties) {
  final radius = BorderRadiusDirectional.horizontal(
    start: properties.getValue<Radius>('start') ?? Radius.zero,
    end: properties.getValue<Radius>('end') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusVertical = IrNativeFunc(borderRadiusVerticalImpl);

Eval<Ir> borderRadiusVerticalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderRadiusVertical(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderRadiusVertical(WidgetProperties properties) {
  final radius = BorderRadiusDirectional.vertical(
    top: properties.getValue<Radius>('top') ?? Radius.zero,
    bottom: properties.getValue<Radius>('bottom') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}

final borderRadiusOnly = IrNativeFunc(borderRadiusOnlyImpl);

Eval<Ir> borderRadiusOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderRadiusOnly(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderRadiusOnly(WidgetProperties properties) {
  final radius = BorderRadius.only(
    topLeft: properties.getValue<Radius>('top-left') ?? Radius.zero,
    topRight: properties.getValue<Radius>('top-right') ?? Radius.zero,
    bottomLeft: properties.getValue<Radius>('bottom-left') ?? Radius.zero,
    bottomRight: properties.getValue<Radius>('botton-right') ?? Radius.zero,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}
