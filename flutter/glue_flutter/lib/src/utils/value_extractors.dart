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
List<T> extractNativeValues<T>(Ir? value) => switch (value) {
  IrList(:final elements) =>
    elements
        .map(
          (child) => switch (child) {
            IrNativeValue(value: Value(value: T widget)) => widget,
            _ => null,
          },
        )
        .whereType<T>()
        .toList(),
  _ => switch (extractNativeValue<T>(value)) {
    null => [],
    T res => [res],
  },
};

/// Extract VoidCallback from Glue IR value with provided runtime
VoidCallback Function(Runtime)? extractVoidCallback(Ir? value) =>
    switch (value) {
      IrClosure(:final params) =>
        params.isEmpty
            ? (Runtime runtime) => () {
                final evalAction = apply(value, []);
                // Use provided runtime instead of creating from env
                final result = runEval(evalAction, runtime);
                switch (result) {
                  case Either<EvalError, (Ir, Runtime)> r:
                    r.match(
                      (error) => print('Callback execution error: $error'),
                      (_) {}, // Success, do nothing
                    );
                }
              }
            : null, // Only support parameterless closures for VoidCallback
      _ => null,
    };

typedef Callback<T> = void Function(T? value);

/// Extract Callback from Glue IR value with provided runtime
Callback<T> Function(Runtime)? extractCallback<T>(Ir? value) => switch (value) {
  IrClosure(:final params) =>
    params.length == 1
        ? (Runtime runtime) => (arg) {
            final args = switch (arg) {
              bool v => [IrBool(v)],
              int v => [IrInteger(v)],
              double v => [IrFloat(v)],
              String v => [IrString(v)],
              T v => [IrNativeValue(Value(v))],
              _ => <Ir>[],
            };
            final evalAction = apply(value, args);
            // Use provided runtime instead of creating from env
            final result = runEval(evalAction, runtime);
            switch (result) {
              case Either<EvalError, (Ir, Runtime)> r:
                r.match(
                  (error) => print('Callback execution error: $error'),
                  (_) {}, // Success, do nothing
                );
            }
          }
        : null, // Only support parameterless closures for VoidCallback
  _ => null,
};
