import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// RGB function - (rgb 255 0 0)
final rgb = IrNativeFunc(rgbImpl);

Eval<Ir> rgbImpl(Ir r) => Eval.pure(
  IrNativeFunc(
    (Ir g) => Eval.pure(IrNativeFunc((Ir b) => createRgbColor(r, g, b))),
  ),
);

Eval<Ir> createRgbColor(Ir r, Ir g, Ir b) {
  final red = extractInt(r)?.clamp(0, 255) ?? 0;
  final green = extractInt(g)?.clamp(0, 255) ?? 0;
  final blue = extractInt(b)?.clamp(0, 255) ?? 0;
  final color = Color.fromARGB(255, red, green, blue);
  return Eval.pure(IrNativeValue(HostValue(color)));
}
