import 'dart:ui';

import 'package:glue/ir.dart';

/// Parse color from Glue IR value
/// Supports named colors, hex codes, and rgb() notation
Color? parseColor(Ir? ir) {
  return switch (ir) {
    IrString(value: final colorStr) => _parseColorString(colorStr),
    _ => null,
  };
}

/// Parse color from string
Color? _parseColorString(String colorStr) {
  // Hex colors (#RGB, #RRGGBB, #AARRGGBB)
  if (colorStr.startsWith('#')) {
    return _parseHexColor(colorStr);
  }
  return null;
}

/// Parse hex color
Color? _parseHexColor(String hex) {
  try {
    final buffer = StringBuffer();
    if (hex.length == 4) {
      // #RGB -> #RRGGBB
      buffer.write('#');
      for (var i = 1; i < hex.length; i++) {
        buffer.write(hex[i] * 2);
      }
    } else {
      buffer.write(hex);
    }

    final colorValue = int.parse(buffer.toString().substring(1), radix: 16);
    if (buffer.toString().length == 7) {
      // #RRGGBB
      return Color(colorValue | 0xFF000000);
    } else if (buffer.toString().length == 9) {
      // #AARRGGBB
      return Color(colorValue);
    }
  } catch (_) {
    // Invalid hex format
  }
  return null;
}
