import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// HSV function - (hsv 360 1.0 1.0)
final hsv = IrNativeFunc(hsvImpl);

Eval<Ir> hsvImpl(Ir h) => Eval.pure(
  IrNativeFunc(
    (Ir s) => Eval.pure(IrNativeFunc((Ir v) => createHsvColor(h, s, v))),
  ),
);

Eval<Ir> createHsvColor(Ir h, Ir s, Ir v) {
  final hue = (extractDouble(h)?.clamp(0, 360) ?? 0).toDouble();
  final saturation = (extractDouble(s)?.clamp(0, 1) ?? 0).toDouble();
  final value = (extractDouble(v)?.clamp(0, 1) ?? 1).toDouble();
  final color = HSVColor.fromAHSV(1.0, hue, saturation, value).toColor();
  return Eval.pure(IrNativeValue(Value(color)));
}
