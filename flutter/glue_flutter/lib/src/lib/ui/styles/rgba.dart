import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// RGBA function - (rgba 255 0 0 128)
final rgba = IrNativeFunc(rgbaImpl);

Eval<Ir> rgbaImpl(Ir r) => Eval.pure(
  IrNativeFunc(
    (Ir g) => Eval.pure(
      IrNativeFunc(
        (Ir b) =>
            Eval.pure(IrNativeFunc((Ir a) => createRgbaColor(r, g, b, a))),
      ),
    ),
  ),
);

Eval<Ir> createRgbaColor(Ir r, Ir g, Ir b, Ir a) {
  final red = extractInt(r)?.clamp(0, 255) ?? 0;
  final green = extractInt(g)?.clamp(0, 255) ?? 0;
  final blue = extractInt(b)?.clamp(0, 255) ?? 0;
  final alpha = extractInt(a)?.clamp(0, 255) ?? 255;
  final color = Color.fromARGB(alpha, red, green, blue);
  return Eval.pure(IrNativeValue(Value(color)));
}
