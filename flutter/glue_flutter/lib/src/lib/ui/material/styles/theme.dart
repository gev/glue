import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

final theme = IrNativeFunc(
  (props) => switch (props) {
    IrObject(:final properties) => _createTheme(
      WidgetProperties(properties.unlock),
    ),
    _ => Eval.pure(IrNativeValue(Value(ThemeData(useMaterial3: true)))),
  },
);

Eval<Ir> _createTheme(WidgetProperties properties) {
  final colorScheme = properties.getValue<ColorScheme>('color-scheme');
  final brightness = properties.getValue<Brightness>('brightness');

  final themeData = ThemeData(
    useMaterial3: true,
    colorScheme: colorScheme,
    brightness: brightness,
  );

  return Eval.pure(IrNativeValue(Value(themeData)));
}
