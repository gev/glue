import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// Font weight enum object - represents all FontWeight values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FontWeight enum value
final fontWeight = IrObject({
  'normal': IrNativeValue(Value(FontWeight.normal)),
  'bold': IrNativeValue(Value(FontWeight.bold)),
  'w100': IrNativeValue(Value(FontWeight.w100)),
  'w200': IrNativeValue(Value(FontWeight.w200)),
  'w300': IrNativeValue(Value(FontWeight.w300)),
  'w400': IrNativeValue(Value(FontWeight.w400)),
  'w500': IrNativeValue(Value(FontWeight.w500)),
  'w600': IrNativeValue(Value(FontWeight.w600)),
  'w700': IrNativeValue(Value(FontWeight.w700)),
  'w800': IrNativeValue(Value(FontWeight.w800)),
  'w900': IrNativeValue(Value(FontWeight.w900)),
});
