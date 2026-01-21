import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Font weight enum object - represents all FontWeight values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FontWeight enum value
final fontWeight = IrObject({
  'normal': IrNativeValue(HostValue(FontWeight.normal)),
  'bold': IrNativeValue(HostValue(FontWeight.bold)),
  'w100': IrNativeValue(HostValue(FontWeight.w100)),
  'w200': IrNativeValue(HostValue(FontWeight.w200)),
  'w300': IrNativeValue(HostValue(FontWeight.w300)),
  'w400': IrNativeValue(HostValue(FontWeight.w400)),
  'w500': IrNativeValue(HostValue(FontWeight.w500)),
  'w600': IrNativeValue(HostValue(FontWeight.w600)),
  'w700': IrNativeValue(HostValue(FontWeight.w700)),
  'w800': IrNativeValue(HostValue(FontWeight.w800)),
  'w900': IrNativeValue(HostValue(FontWeight.w900)),
});
