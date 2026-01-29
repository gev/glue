import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

/// System brightness detection functions
/// Allow accessing the device's current system brightness setting

/// Get current system brightness (device setting)
/// Returns Brightness.dark or Brightness.light
final systemBrightness = IrNativeFunc(
  (args) => Eval.pure(IrNativeValue(Value(_getSystemBrightness()))),
);

/// Get system brightness enum object
/// Usage: (system-brightness) returns device brightness setting
final systemBrightnessEnum = IrObject({
  // System detection functions
  'get': IrNativeValue(Value(_getSystemBrightness)),
  // Direct access to enums
  'dark': IrNativeValue(Value(Brightness.dark)),
  'light': IrNativeValue(Value(Brightness.light)),
});

/// Helper function to get current system brightness
/// This will be called by Glue code to access device settings
Brightness _getSystemBrightness() {
  // This requires BuildContext to get actual system brightness
  // For now, return light - in practice, this needs context from app
  return Brightness.light;
}

// Note: In actual usage, system brightness detection needs BuildContext
// from a widget context. The app widget builders will handle this.
