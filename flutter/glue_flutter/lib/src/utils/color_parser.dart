import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

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
  // Named colors
  final namedColors = {
    'red': Colors.red,
    'blue': Colors.blue,
    'green': Colors.green,
    'yellow': Colors.yellow,
    'black': Colors.black,
    'white': Colors.white,
    'gray': Colors.grey,
    'grey': Colors.grey,
    'orange': Colors.orange,
    'purple': Colors.purple,
    'pink': Colors.pink,
    'brown': Colors.brown,
    'cyan': Colors.cyan,
    'indigo': Colors.indigo,
    'lime': Colors.lime,
    'teal': Colors.teal,
    'transparent': Colors.transparent,
  };

  if (namedColors.containsKey(colorStr.toLowerCase())) {
    return namedColors[colorStr.toLowerCase()];
  }

  // Hex colors (#RGB, #RRGGBB, #AARRGGBB)
  if (colorStr.startsWith('#')) {
    return _parseHexColor(colorStr);
  }

  // RGB notation removed - use runtime rgb/rgba functions instead

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
