import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Material widget function
/// Creates Flutter Material from Glue (material props) expressions
final Ir material = IrNativeFunc(materialImpl);

/// Material implementation - takes properties object
Eval<Ir> materialImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createMaterial(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Material widget from properties
Eval<Ir> _createMaterial(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`Widget` child property required']));
  }
  final materialWidget = Material(
    key: properties.key,
    type: properties.getValue<MaterialType>('type') ?? MaterialType.canvas,
    elevation: properties.getDouble('elevation') ?? 0.0,
    color: properties.getValue<Color>('color'),
    shadowColor: properties.getValue<Color>('shadow-color'),
    surfaceTintColor: properties.getValue<Color>('surfave-tint-color'),
    textStyle: properties.getValue<TextStyle>('text-style'),
    borderRadius: properties.getValue<BorderRadiusGeometry>('border-radius'),
    shape: properties.getValue<ShapeBorder>('shape'),
    borderOnForeground: properties.getBool('border-on-foreground') ?? true,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    animationDuration:
        properties.getValue<Duration>('animatioln-duration') ??
        kThemeChangeDuration,
    animateColor: properties.getBool('animate-color') ?? false,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(materialWidget)));
}
