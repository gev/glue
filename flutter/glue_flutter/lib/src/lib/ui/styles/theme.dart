import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Theme constructor - create custom ThemeData from properties
/// Usage: (theme {:colorScheme color-scheme :primaryColor primary-color :accentColor secondary-color})
final theme = IrNativeFunc(themeImpl);

Eval<Ir> themeImpl(Ir props) => createThemeData(props);

Eval<Ir> createThemeData(Ir props) {
  if (props is IrObject) {
    // Simple implementation for now - extract basic properties
    final primaryColor = extractColorSimple(props.getValue('primaryColor'));
    final scaffoldBackgroundColor = extractColorSimple(
      props.getValue('scaffoldBackgroundColor'),
    );

    final themeData = ThemeData(
      primaryColor: primaryColor,
      scaffoldBackgroundColor: scaffoldBackgroundColor,
    );

    return Eval.pure(IrNativeValue(Value(themeData)));
  }

  return Eval.pure(IrNativeValue(Value(ThemeData())));
}

// Simple color extractor - convert from Glue IR to Flutter Color
Color? extractColorSimple(dynamic value) {
  if (value is IrNativeValue && value.value.value is Color) {
    return value.value.value as Color;
  }
  return null;
}
