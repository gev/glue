import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/color.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// HSLA function - (hsla 360 1.0 0.5 0.8)
final hsla = IrNativeFunc(hslaImpl);

Eval<Ir> hslaImpl(Ir h) => Eval.pure(
  IrNativeFunc(
    (Ir s) => Eval.pure(
      IrNativeFunc(
        (Ir l) =>
            Eval.pure(IrNativeFunc((Ir a) => createHslaColor(h, s, l, a))),
      ),
    ),
  ),
);

Eval<Ir> createHslaColor(Ir h, Ir s, Ir l, Ir a) {
  final hue = (extractDouble(h)?.clamp(0, 360) ?? 0).toDouble();
  final saturation = (extractDouble(s)?.clamp(0, 1) ?? 0).toDouble();
  final lightness = (extractDouble(l)?.clamp(0, 1) ?? 0).toDouble();
  final alpha = (extractDouble(a)?.clamp(0, 1) ?? 1).toDouble();
  final color = HSLColor.fromAHSL(alpha, hue, saturation, lightness).toColor();
  return Eval.pure(makeColor(color));
}
