import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// Colors object - represents common named colors as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Color constant
final colors = IrObject({
  // Material Design colors
  'red': IrNativeValue(Value(Colors.red)),
  'pink': IrNativeValue(Value(Colors.pink)),
  'purple': IrNativeValue(Value(Colors.purple)),
  'deepPurple': IrNativeValue(Value(Colors.deepPurple)),
  'indigo': IrNativeValue(Value(Colors.indigo)),
  'blue': IrNativeValue(Value(Colors.blue)),
  'lightBlue': IrNativeValue(Value(Colors.lightBlue)),
  'cyan': IrNativeValue(Value(Colors.cyan)),
  'teal': IrNativeValue(Value(Colors.teal)),
  'green': IrNativeValue(Value(Colors.green)),
  'lightGreen': IrNativeValue(Value(Colors.lightGreen)),
  'lime': IrNativeValue(Value(Colors.lime)),
  'yellow': IrNativeValue(Value(Colors.yellow)),
  'amber': IrNativeValue(Value(Colors.amber)),
  'orange': IrNativeValue(Value(Colors.orange)),
  'deepOrange': IrNativeValue(Value(Colors.deepOrange)),
  'brown': IrNativeValue(Value(Colors.brown)),
  'grey': IrNativeValue(Value(Colors.grey)),
  'blueGrey': IrNativeValue(Value(Colors.blueGrey)),

  // Common shades
  'black': IrNativeValue(Value(Colors.black)),
  'white': IrNativeValue(Value(Colors.white)),
  'transparent': IrNativeValue(Value(Colors.transparent)),

  // Accent colors
  'redAccent': IrNativeValue(Value(Colors.redAccent)),
  'pinkAccent': IrNativeValue(Value(Colors.pinkAccent)),
  'purpleAccent': IrNativeValue(Value(Colors.purpleAccent)),
  'deepPurpleAccent': IrNativeValue(Value(Colors.deepPurpleAccent)),
  'indigoAccent': IrNativeValue(Value(Colors.indigoAccent)),
  'blueAccent': IrNativeValue(Value(Colors.blueAccent)),
  'lightBlueAccent': IrNativeValue(Value(Colors.lightBlueAccent)),
  'cyanAccent': IrNativeValue(Value(Colors.cyanAccent)),
  'tealAccent': IrNativeValue(Value(Colors.tealAccent)),
  'greenAccent': IrNativeValue(Value(Colors.greenAccent)),
  'lightGreenAccent': IrNativeValue(Value(Colors.lightGreenAccent)),
  'limeAccent': IrNativeValue(Value(Colors.limeAccent)),
  'yellowAccent': IrNativeValue(Value(Colors.yellowAccent)),
  'amberAccent': IrNativeValue(Value(Colors.amberAccent)),
  'orangeAccent': IrNativeValue(Value(Colors.orangeAccent)),
  'deepOrangeAccent': IrNativeValue(Value(Colors.deepOrangeAccent)),
});
