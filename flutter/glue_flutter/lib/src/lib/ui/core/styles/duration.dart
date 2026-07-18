import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final duration = IrNativeFunc(
  (Ir ir) => switch (ir) {
    IrObject(:final properties) => Eval.pure(
      IrNativeValue(
        Value(
          Duration(
            days: toInt(properties['days']) ?? 0,
            hours: toInt(properties['hours']) ?? 0,
            minutes: toInt(properties['minutes']) ?? 0,
            seconds: toInt(properties['seconds']) ?? 0,
            milliseconds: toInt(properties['milliseconds']) ?? 0,
            microseconds: toInt(properties['microseconds']) ?? 0,
          ),
        ),
      ),
    ),
    _ => throwError(wrongArgumentType(['`Object` properties required'])),
  },
);
