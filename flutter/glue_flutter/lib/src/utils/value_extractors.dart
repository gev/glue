import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Utility functions for extracting values from Glue IR
/// All extraction functions use pattern matching for clean, type-safe code

/// Extract string from Glue IR value
String? extractString(dynamic value) => switch (value) {
  IrString(:final value) => value,
  String string => string,
  _ => null,
};

/// Extract bool from Glue IR value
bool? extractBool(dynamic value) => switch (value) {
  IrBool(:final value) => value,
  bool boolean => boolean,
  _ => null,
};

/// Extract int from Glue IR value
int? extractInt(dynamic value) => switch (value) {
  IrInteger(:final value) => value,
  int integer => integer,
  _ => null,
};

/// Extract double from Glue IR value
double? extractDouble(dynamic value) => switch (value) {
  IrInteger(:final value) => value.toDouble(),
  IrFloat(:final value) => value,
  double d => d,
  int i => i.toDouble(),
  _ => null,
};

/// Extract color from Glue IR value
Color? extractColor(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: Color color)) => color,
  IrString(:final value) => _parseHexColor(value),
  _ => null,
};

/// Parse hex color string (e.g., "#FF0000" or "#FF0000FF")
Color? _parseHexColor(String hexString) {
  if (!hexString.startsWith('#')) return null;

  try {
    // Remove # and parse as hex
    final hexValue = int.parse(hexString.substring(1), radix: 16);

    // Handle different formats
    if (hexString.length == 7) {
      // #RRGGBB - add full alpha
      return Color(0xFF000000 | hexValue);
    } else if (hexString.length == 9) {
      // #AARRGGBB - use as-is
      return Color(hexValue);
    }
  } catch (_) {
    // Invalid hex format
  }

  return null;
}

/// Extract FontWeight from Glue IR value
FontWeight? extractFontWeight(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: FontWeight weight)) => weight,
  _ => null,
};

/// Extract TextAlign from Glue IR value
TextAlign? extractTextAlign(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: TextAlign align)) => align,
  _ => null,
};

/// Extract MainAxisAlignment from Glue IR value
MainAxisAlignment? extractMainAxisAlignment(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: MainAxisAlignment alignment)) =>
    alignment,
  _ => null,
};

/// Extract CrossAxisAlignment from Glue IR value
CrossAxisAlignment? extractCrossAxisAlignment(dynamic value) => switch (value) {
  IrNativeValue(value: HostValue(value: CrossAxisAlignment alignment)) =>
    alignment,
  _ => null,
};

/// Extract children list from Glue IR value
List<Widget>? extractChildren(dynamic value) => switch (value) {
  List list =>
    list
        .map(
          (child) => switch (child) {
            IrNativeValue(value: HostValue(value: Widget widget)) => widget,
            _ => const SizedBox.shrink(),
          },
        )
        .toList(),
  _ => null,
};

/// Extract Axis from Glue IR value
Axis? extractAxis(dynamic value) => switch (value) {
  IrString(value: 'horizontal') => Axis.horizontal,
  IrString(value: 'vertical') => Axis.vertical,
  _ => null,
};

/// Extract VoidCallback from Glue IR value
VoidCallback? extractVoidCallback(dynamic value) => switch (value) {
  // TODO: Implement callback extraction from IrClosure
  _ => null,
};

/// Extract EdgeInsetsGeometry from Glue IR value
EdgeInsetsGeometry? extractEdgeInsets(dynamic value) => switch (value) {
  // TODO: Implement complex EdgeInsets parsing from Glue IR
  // For now, just return a default
  _ => null,
};
