import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Main-axis alignment enum object - represents all MainAxisAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter MainAxisAlignment enum value
final mainAxisAlignment = IrObject({
  'start': IrNativeValue(HostValue(MainAxisAlignment.start)),
  'end': IrNativeValue(HostValue(MainAxisAlignment.end)),
  'center': IrNativeValue(HostValue(MainAxisAlignment.center)),
  'spaceBetween': IrNativeValue(HostValue(MainAxisAlignment.spaceBetween)),
  'spaceAround': IrNativeValue(HostValue(MainAxisAlignment.spaceAround)),
  'spaceEvenly': IrNativeValue(HostValue(MainAxisAlignment.spaceEvenly)),
});
