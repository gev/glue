import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// FloatingActionButtonLocation enum object - represents all FloatingActionButtonLocation values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FloatingActionButtonLocation enum value
final floatingActionButtonLocation = IrObject({
  'start-top': IrNativeValue(Value(FloatingActionButtonLocation.startTop)),
  'mini-start-top': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartTop),
  ),
  'center-top': IrNativeValue(Value(FloatingActionButtonLocation.centerTop)),
  'mini-center-top': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterTop),
  ),
  'end-top': IrNativeValue(Value(FloatingActionButtonLocation.endTop)),
  'mini-end-top': IrNativeValue(Value(FloatingActionButtonLocation.miniEndTop)),
  'start-float': IrNativeValue(Value(FloatingActionButtonLocation.startFloat)),
  'mini-start-float': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartFloat),
  ),
  'center-float': IrNativeValue(
    Value(FloatingActionButtonLocation.centerFloat),
  ),
  'mini-center-float': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterFloat),
  ),
  'end-float': IrNativeValue(Value(FloatingActionButtonLocation.endFloat)),
  'mini-end-float': IrNativeValue(
    Value(FloatingActionButtonLocation.miniEndFloat),
  ),
  'start-docked': IrNativeValue(
    Value(FloatingActionButtonLocation.startDocked),
  ),
  'mini-start-docked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniStartDocked),
  ),
  'center-docked': IrNativeValue(
    Value(FloatingActionButtonLocation.centerDocked),
  ),
  'mini-center-docked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniCenterDocked),
  ),
  'end-docked': IrNativeValue(Value(FloatingActionButtonLocation.endDocked)),
  'mini-end-docked': IrNativeValue(
    Value(FloatingActionButtonLocation.miniEndDocked),
  ),
});
