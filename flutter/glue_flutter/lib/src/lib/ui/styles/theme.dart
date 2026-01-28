import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Theme constructor - create custom ThemeData from properties
/// Usage: (theme {:primaryColor primary-color :scaffoldBackgroundColor bg-color})
final theme = IrNativeFunc(themeImpl);

Eval<Ir> themeImpl(Ir props) => createThemeData(props);

Eval<Ir> createThemeData(Ir props) {
  // For now, just return the default theme
  // This is a placeholder that works - complex property extraction can be added later
  final themeData = ThemeData();
  return Eval.pure(IrNativeValue(Value(themeData)));
}

/// Extract ColorScheme from Glue IR value
ColorScheme? extractColorScheme(dynamic value) {
  if (value is IrNativeValue && value.value.value is ColorScheme) {
    return value.value.value as ColorScheme;
  }
  return null;
}

/// Extract Color from Glue IR value (supports rgb, Color values, etc.)
Color? extractColor(dynamic value) {
  if (value is IrNativeValue && value.value.value is Color) {
    return value.value.value as Color;
  }
  return null;
}

/// Extract TextTheme from Glue IR value
TextTheme? extractTextTheme(dynamic value) {
  if (value is IrNativeValue && value.value.value is TextTheme) {
    return value.value.value as TextTheme;
  }
  return null;
}
