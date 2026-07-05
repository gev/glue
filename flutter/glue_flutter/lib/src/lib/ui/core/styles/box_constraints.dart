import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

/// Box constraints only function - (box-constraints-only (:min-width 0 :max-width 100 :min-height 0 :max-height 100))
final boxConstraintsOnly = IrNativeFunc(boxConstraintsImpl);

Eval<Ir> boxConstraintsImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBoxConstraints(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBoxConstraints(WidgetProperties properties) {
  final constraints = BoxConstraints(
    minWidth: properties.getDouble('min-width') ?? 0.0,
    maxWidth: properties.getDouble('max-width') ?? double.infinity,
    minHeight: properties.getDouble('min-height') ?? 0.0,
    maxHeight: properties.getDouble('max-height') ?? double.infinity,
  );
  return Eval.pure(IrNativeValue(Value(constraints)));
}

/// Box constraints tight function - (box-constraints-tight (:width 100 :height 50))
final boxConstraintsTight = IrNativeFunc(boxConstraintsTightImpl);

Eval<Ir> boxConstraintsTightImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBoxConstraintsTight(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBoxConstraintsTight(WidgetProperties properties) {
  final constraints = BoxConstraints.tightFor(
    width: properties.width,
    height: properties.height,
  );
  return Eval.pure(IrNativeValue(Value(constraints)));
}
