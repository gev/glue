import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final Ir clipRRect = IrNativeFunc((props) {
  return switch (props) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          ClipRRect(
            key: to<Key>(properties['key']),
            borderRadius:
                to<BorderRadiusGeometry>(properties['border-radius']) ??
                BorderRadius.zero,
            clipper: to<CustomClipper<RRect>>(properties['clipper']),
            clipBehavior:
                to<Clip>(properties['clip-behavior']) ?? Clip.antiAlias,
            child: to<Widget>(properties['child']),
          ),
        ),
      ),
    ),
    _ => throwError(
      wrongArgumentType(['`Object` border-radius and child required']),
    ),
  };
});
