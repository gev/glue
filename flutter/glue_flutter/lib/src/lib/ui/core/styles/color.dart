import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

Ir makeColor(Color? c) {
  if (c == null) return IrVoid();
  return IrNativeValue(
    Value(
      c,
      getters: {
        'alpha': _float(c.a),
        'r': _float(c.r),
        'g': _float(c.g),
        'b': _float(c.b),
        'luminance': _float(c.computeLuminance()),
        'with': _with(c),
      },
    ),
  );
}

Eval<Ir> _float(double v) {
  return Eval.pure(IrFloat(v));
}

Eval<Ir> _with<T>(Color c) {
  return Eval.pure(
    IrNativeFunc((props) {
      return switch (props) {
        IrObject(:final properties) => Eval.pure(
          makeColor(
            c.withValues(
              alpha: toDouble(properties['a']),
              red: toDouble(properties['r']),
              green: toDouble(properties['g']),
              blue: toDouble(properties['b']),
            ),
          ),
        ),
        _ => throwError(
          wrongArgumentType(['`Object` color properties required']),
        ),
      };
    }),
  );
}
