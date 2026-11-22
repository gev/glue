import 'dart:math';

import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final Ir linearGradient = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          LinearGradient(
            begin:
                to<AlignmentGeometry>(properties['begin']) ??
                Alignment.centerLeft,
            end:
                to<AlignmentGeometry>(properties['end']) ??
                Alignment.centerRight,
            colors: toList<Color>(properties['colors']),
            stops: toList<double>(properties['stops']),
            tileMode: to<TileMode>(properties['tile-mode']) ?? TileMode.clamp,
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` properties required for linear-gradient']),
    ),
  };
});

final Ir radialGradient = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          RadialGradient(
            center:
                to<AlignmentGeometry>(properties['center']) ?? Alignment.center,
            radius: to<double>(properties['radius']) ?? 0.5,
            colors: toList<Color>(properties['colors']),
            stops: toList<double>(properties['stops']),
            tileMode: to<TileMode>(properties['tile-mode']) ?? TileMode.clamp,
            focal: to<AlignmentGeometry>(properties['focal']),
            focalRadius: to<double>(properties['focal-radius']) ?? 0.0,
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` properties required for radial-gradient']),
    ),
  };
});

final Ir sweepGradient = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          SweepGradient(
            center:
                to<AlignmentGeometry>(properties['center']) ?? Alignment.center,
            startAngle: to<double>(properties['start-angle']) ?? 0.0,
            endAngle: to<double>(properties['end-angle']) ?? pi * 2,
            colors: toList<Color>(properties['colors']),
            stops: toList<double>(properties['stops']),
            tileMode: to<TileMode>(properties['tile-mode']) ?? TileMode.clamp,
            transform: to<GradientTransform>(properties['transform']),
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` properties required for sweep-gradient']),
    ),
  };
});
