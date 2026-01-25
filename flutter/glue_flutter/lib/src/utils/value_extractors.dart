import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/error.dart';
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

/// Extract color from Glue IR value
Color? extractColor(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: Color color)) => color,
  IrString() => parseColor(value),
  _ => null,
};

/// Extract FontWeight from Glue IR value
FontWeight? extractFontWeight(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: FontWeight weight)) => weight,
  _ => null,
};

/// Extract TextAlign from Glue IR value
TextAlign? extractTextAlign(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: TextAlign align)) => align,
  _ => null,
};

/// Extract MainAxisAlignment from Glue IR value
MainAxisAlignment? extractMainAxisAlignment(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: MainAxisAlignment alignment)) =>
    alignment,
  _ => null,
};

/// Extract CrossAxisAlignment from Glue IR value
CrossAxisAlignment? extractCrossAxisAlignment(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: CrossAxisAlignment alignment)) =>
    alignment,
  _ => null,
};

/// Extract child list from Glue IR value
Widget? extractChild(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: Widget widget)) => widget,
  _ => null,
};

/// Extract children list from Glue IR value
List<Widget>? extractChildren(Ir? value) => switch (value) {
  IrList(:final elements) =>
    elements
        .map(
          (child) => switch (child) {
            IrNativeValue(value: HostValue(value: Widget widget)) => widget,
            _ => null,
          },
        )
        .whereType<Widget>()
        .toList(),
  _ => null,
};

/// Extract Axis from Glue IR value
Axis? extractAxis(Ir? value) => switch (value) {
  IrString(value: 'horizontal') => Axis.horizontal,
  IrString(value: 'vertical') => Axis.vertical,
  _ => null,
};

/// Extract VoidCallback from Glue IR value with provided runtime
VoidCallback? extractVoidCallback(Ir? value, Runtime runtime) =>
    switch (value) {
      IrClosure(:final params) =>
        params.isEmpty
            ? () async {
                final evalAction = apply(value, []);
                // Use provided runtime instead of creating from env
                final result = await runEval(evalAction, runtime);
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

/// Extract EdgeInsetsGeometry from Glue IR value
EdgeInsetsGeometry? extractEdgeInsets(Ir? value) => switch (value) {
  IrNativeValue(value: HostValue(value: EdgeInsetsGeometry insets)) => insets,
  _ => null,
};
