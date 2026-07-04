import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

final radiusZero = IrNativeValue(Value(Radius.zero));

final radiusCircular = IrNativeFunc(radiusCircularImpl);

Eval<Ir> radiusCircularImpl(Ir value) {
  final radius = extractDouble(value);
  if (radius == null) return throwError(wrongArgumentType(['number']));
  return createRadiusCircular(Radius.circular(radius));
}

Eval<Ir> createRadiusCircular(Radius radius) =>
    Eval.pure(IrNativeValue(Value(radius)));

final radiusElliptical = IrNativeFunc(radiusEllipticalImpl);

Eval<Ir> radiusEllipticalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createRadiusElliptical(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createRadiusElliptical(WidgetProperties properties) {
  final radius = Radius.elliptical(
    properties.getValue<double>('x') ?? 0.0,
    properties.getValue<double>('y') ?? 0.0,
  );
  return Eval.pure(IrNativeValue(Value(radius)));
}
