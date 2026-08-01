import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

/// Border all function - (border-all (:color 0xFF000000 :width 1.0))
final borderAll = IrNativeFunc(borderAllImpl);

Eval<Ir> borderAllImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderAll(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderAll(WidgetProperties properties) {
  return Eval.pure(
    IrNativeValue(
      Value(
        Border.all(
          color: properties.getValue<Color>('color') ?? Color(0xFF000000),
          width: properties.width ?? 1,
        ),
      ),
    ),
  );
}

/// Border symmetric function - (bordersymmetric (:vertical (:color 0xFF000000 :width 1.0) :horizontal (:color 0xFF000000 :width 1.0)))
final borderSymmetric = IrNativeFunc(borderSymmetricImpl);

Eval<Ir> borderSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderSymmetric(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderSymmetric(WidgetProperties properties) {
  return Eval.pure(
    IrNativeValue(
      Value(
        Border.symmetric(
          vertical:
              properties.getValue<BorderSide>('vertical') ?? BorderSide.none,
          horizontal:
              properties.getValue<BorderSide>('horizontal') ?? BorderSide.none,
        ),
      ),
    ),
  );
}

/// Border only function - (border-only (:top (:color 0xFF000000 :width 1.0) :left ... :right ... :bottom ...))
final borderOnly = IrNativeFunc(borderOnlyImpl);

Eval<Ir> borderOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createBorderOnly(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createBorderOnly(WidgetProperties properties) {
  BorderSide parseSide(dynamic value) {
    if (value is BorderSide) return value;
    return BorderSide.none;
  }

  return Eval.pure(
    IrNativeValue(
      Value(
        Border(
          top: parseSide(properties.top),
          left: parseSide(properties.left),
          right: parseSide(properties.right),
          bottom: parseSide(properties.bottom),
        ),
      ),
    ),
  );
}
