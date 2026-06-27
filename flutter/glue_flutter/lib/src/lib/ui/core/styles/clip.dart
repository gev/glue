import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Clip enum object - represents all Clip values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Clip enum value
final clip = IrObject({
  'none': IrNativeValue(Value(Clip.none)),
  'hard-edge': IrNativeValue(Value(Clip.hardEdge)),
  'anti-alias': IrNativeValue(Value(Clip.antiAlias)),
  'anti-alias-with-save-layer': IrNativeValue(
    Value(Clip.antiAliasWithSaveLayer),
  ),
});
