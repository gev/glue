import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// FloatingActionButtonLocation enum object - represents all FloatingActionButtonLocation values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FloatingActionButtonLocation enum value
final floatingActionButtonLocation = IrObject({
  'startTop': IrNativeValue(Value(FloatingActionButtonLocation.startTop)),
  'miniStartTop': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartTop),
  ),
  'centerTop': IrNativeValue(Value(FloatingActionButtonLocation.centerTop)),
  'miniCenterTop': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterTop),
  ),
  'endTop': IrNativeValue(Value(FloatingActionButtonLocation.endTop)),
  'miniEndTop': IrNativeValue(Value(FloatingActionButtonLocation.miniEndTop)),
  'startFloat': IrNativeValue(Value(FloatingActionButtonLocation.startFloat)),
  'miniStartFloat': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartFloat),
  ),
  'centerFloat': IrNativeValue(Value(FloatingActionButtonLocation.centerFloat)),
  'miniCenterFloat': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterFloat),
  ),
  'endFloat': IrNativeValue(Value(FloatingActionButtonLocation.endFloat)),
  'miniEndFloat': IrNativeValue(
    Value(FloatingActionButtonLocation.miniEndFloat),
  ),
  'startDocked': IrNativeValue(Value(FloatingActionButtonLocation.startDocked)),
  'miniStartDocked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartDocked),
  ),
  'centerDocked': IrNativeValue(
    Value(FloatingActionButtonLocation.centerDocked),
  ),
  'miniCenterDocked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterDocked),
  ),
  'endDocked': IrNativeValue(Value(FloatingActionButtonLocation.endDocked)),
  'miniEndDocked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniEndDocked),
  ),
});
