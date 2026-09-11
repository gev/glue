import 'package:flutter/widgets.dart';
import 'package:glue/either.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/color_parser.dart';

/// Utility functions for extracting values from Glue IR
/// All extraction functions use pattern matching for clean, type-safe code

/// Extract string from Glue IR value
String? extractString(Ir? value) => switch (value) {
  IrString(:final value) => value,
  IrInteger(:final value) => value.toString(),
  IrFloat(:final value) => value.toString(),
  _ => null,
};

/// Extract bool from Glue IR value
bool? extractBool(Ir? value) => switch (value) {
  IrBool(:final value) => value,
  _ => null,
};

/// Extract int from Glue IR value
int? extractInt(Ir? value) => switch (value) {
  IrInteger(:final value) => value,
  _ => null,
};

/// Extract double from Glue IR value
double? extractDouble(Ir? value) => switch (value) {
  IrInteger(:final value) => value.toDouble(),
  IrFloat(:final value) => value,
  _ => null,
};

T? extractNativeValue<T>(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: T v)) => v,
  _ => null,
};

/// Extract color from Glue IR value
Color? extractColor(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Color color)) => color,
  IrString() => parseColor(value),
  _ => null,
};

/// Extract children list from Glue IR value
List<T> extractNativeValues<T>(Ir? value) {
  switch (value) {
    case IrList(:final elements):
      final res = <T>[];
      for (final element in elements) {
        if (element case IrNativeValue(value: Value(value: final v))) {
          if (v is T) res.add(v);
        }
      }
      return res;
    default:
      final res = (extractNativeValue<T>(value));
      return res != null ? [res] : [];
  }
}

/// Extract VoidCallback from Glue IR value with provided runtime
VoidCallback Function(Runtime)? extractVoidCallback(Ir? value) =>
    value != null && isCallable(value)
    ? (Runtime runtime) => () {
        final evalAction = apply(value, []);
        final result = runEval(evalAction, runtime);
        switch (result) {
          case Either<EvalError, (Ir, Runtime)> r:
            r.match(
              (error) => print('Callback execution error: $error'),
              (_) {}, // Success, do nothing
            );
        }
      }
    : null;

typedef Callback<T> = void Function(T? value);

/// Extract Callback from Glue IR value with provided runtime
Callback<T> Function(Runtime)? extractCallback<T>(Ir? value) =>
    value != null && isCallable(value)
    ? (Runtime runtime) => (T? arg) {
        final args = switch (arg) {
          bool v => [IrBool(v)],
          int v => [IrInteger(v)],
          double v => [IrFloat(v)],
          String v => [IrString(v)],
          T v => [IrNativeValue(Value(v))],
          _ => <Ir>[],
        };
        final evalAction = apply(value, args);
        final result = runEval(evalAction, runtime);
        switch (result) {
          case Either<EvalError, (Ir, Runtime)> r:
            r.match(
              (error) => print('Callback execution error: $error'),
              (_) {}, // Success, do nothing
            );
        }
      }
    : null;
