import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final visualDensityStandard = IrNativeValue(Value(VisualDensity.standard));

final visualDensityCompact = IrNativeValue(Value(VisualDensity.compact));

final visualDensityComfortable = IrNativeValue(
  Value(VisualDensity.comfortable),
);

final visualDensity = IrNativeFunc(visualDensityImpl);

Eval<Ir> visualDensityImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createVisualDensity(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createVisualDensity(WidgetProperties properties) {
  final density = VisualDensity(
    horizontal: properties.horizontal ?? 0.0,
    vertical: properties.vertical ?? 0.0,
  );
  return Eval.pure(IrNativeValue(Value(density)));
}
