import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse color from Glue IR value
/// Supports named colors, hex codes, and rgb() notation
Color? parseColor(Ir ir) {
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

  // RGB notation rgb(r,g,b) or rgba(r,g,b,a)
  if (colorStr.toLowerCase().startsWith('rgb')) {
    return _parseRgbColor(colorStr);
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

/// Parse RGB color
Color? _parseRgbColor(String rgb) {
  try {
    final rgbRegex = RegExp(
      r'rgba?\((\d+),\s*(\d+),\s*(\d+)(?:,\s*([\d.]+))?\)',
    );
    final match = rgbRegex.firstMatch(rgb.toLowerCase());
    if (match != null) {
      final r = int.parse(match.group(1)!).clamp(0, 255);
      final g = int.parse(match.group(2)!).clamp(0, 255);
      final b = int.parse(match.group(3)!).clamp(0, 255);
      final a = match.group(4) != null
          ? (double.parse(match.group(4)!) * 255).round().clamp(0, 255)
          : 255;

      return Color.fromARGB(a, r, g, b);
    }
  } catch (_) {
    // Invalid RGB format
  }
  return null;
}
