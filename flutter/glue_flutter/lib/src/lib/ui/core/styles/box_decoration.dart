import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final Ir boxDecoration = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          BoxDecoration(
            color: to<Color>(properties['color']),
            image: to<DecorationImage>(properties['image']),
            border: to<BoxBorder>(properties['border']),
            borderRadius: to<BorderRadiusGeometry>(properties['border-radius']),
            boxShadow: toList<BoxShadow>(properties['box-shadow']),
            gradient: to<Gradient>(properties['gradient']),
            backgroundBlendMode: to<BlendMode>(
              properties['background-blend-mode'],
            ),
            shape: to<BoxShape>(properties['shape']) ?? BoxShape.rectangle,
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` properties required for box-decoration']),
    ),
  };
});
