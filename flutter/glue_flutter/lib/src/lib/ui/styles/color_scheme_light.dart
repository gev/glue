import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Pre-configured Material 3 light ColorScheme
/// Based on the default Material 3 light color scheme
final colorSchemeLight = IrNativeValue(Value(_createLightColorScheme()));

ColorScheme _createLightColorScheme() {
  return const ColorScheme(
    brightness: Brightness.light,
    primary: Color(0xff6750a4),
    onPrimary: Color(0xffffffff),
    primaryContainer: Color(0xffe9ddff),
    onPrimaryContainer: Color(0xff22005d),
    secondary: Color(0xff625b71),
    onSecondary: Color(0xffffffff),
    secondaryContainer: Color(0xffe8def8),
    onSecondaryContainer: Color(0xff1e192b),
    tertiary: Color(0xff7d5260),
    onTertiary: Color(0xffffffff),
    tertiaryContainer: Color(0xffffd9e3),
    onTertiaryContainer: Color(0xff31101d),
    error: Color(0xffba1a1a),
    onError: Color(0xffffffff),
    errorContainer: Color(0xffffdad6),
    onErrorContainer: Color(0xff410002),
    surface: Color(0xfffefcff),
    onSurface: Color(0xff1c1b1e),
    surfaceVariant: Color(0xffe4e1ec),
    surfaceContainerHighest: Color(0xffe4e1ec),
    onSurfaceVariant: Color(0xff47464f),
    outline: Color(0xff787680),
    onInverseSurface: Color(0xfff4f0f4),
    inverseSurface: Color(0xff313033),
    inversePrimary: Color(0xffcfbcff),
    shadow: Color(0xff000000),
    surfaceTint: Color(0xff6750a4),
  );
}
