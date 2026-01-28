import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Clip enum object - represents all Clip values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Clip enum value
final clip = IrObject({
  'none': IrNativeValue(Value(Clip.none)),
  'hardEdge': IrNativeValue(Value(Clip.hardEdge)),
  'antiAlias': IrNativeValue(Value(Clip.antiAlias)),
  'antiAliasWithSaveLayer': IrNativeValue(Value(Clip.antiAliasWithSaveLayer)),
});
