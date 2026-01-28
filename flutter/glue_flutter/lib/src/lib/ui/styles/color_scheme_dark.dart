import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Pre-configured Material 3 dark ColorScheme
/// Based on the default Material 3 dark color scheme
final colorSchemeDark = IrNativeValue(Value(_createDarkColorScheme()));

ColorScheme _createDarkColorScheme() {
  return const ColorScheme(
    brightness: Brightness.dark,
    primary: Color(0xffcfbcff),
    onPrimary: Color(0xff381e72),
    primaryContainer: Color(0xff4f378a),
    onPrimaryContainer: Color(0xffe9ddff),
    secondary: Color(0xffcbc2db),
    onSecondary: Color(0xff332d41),
    secondaryContainer: Color(0xff4a4458),
    onSecondaryContainer: Color(0xffe8def8),
    tertiary: Color(0xffefb8c8),
    onTertiary: Color(0xff4a2532),
    tertiaryContainer: Color(0xff633b48),
    onTertiaryContainer: Color(0xffffd9e3),
    error: Color(0xffffb4ab),
    onError: Color(0xff690005),
    errorContainer: Color(0xff93000a),
    onErrorContainer: Color(0xffffdad6),
    background: Color(0xff141218),
    onBackground: Color(0xffe6e1e5),
    surface: Color(0xff141218),
    onSurface: Color(0xffe6e1e5),
    surfaceVariant: Color(0xff47464f),
    onSurfaceVariant: Color(0xffcac4d0),
    outline: Color(0xff948f99),
    onInverseSurface: Color(0xff322f35),
    inverseSurface: Color(0xffe6e1e5),
    inversePrimary: Color(0xff6750a4),
    shadow: Color(0xff000000),
    surfaceTint: Color(0xffcfbcff),
  );
}
