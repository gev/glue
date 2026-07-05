import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// Colors object - represents common named colors as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Color constant
final colors = IrObject({
  // Material Design colors
  'red': IrNativeValue(Value(Colors.red)),
  'pink': IrNativeValue(Value(Colors.pink)),
  'purple': IrNativeValue(Value(Colors.purple)),
  'deep-purple': IrNativeValue(Value(Colors.deepPurple)),
  'indigo': IrNativeValue(Value(Colors.indigo)),
  'blue': IrNativeValue(Value(Colors.blue)),
  'light-blue': IrNativeValue(Value(Colors.lightBlue)),
  'cyan': IrNativeValue(Value(Colors.cyan)),
  'teal': IrNativeValue(Value(Colors.teal)),
  'green': IrNativeValue(Value(Colors.green)),
  'light-green': IrNativeValue(Value(Colors.lightGreen)),
  'lime': IrNativeValue(Value(Colors.lime)),
  'yellow': IrNativeValue(Value(Colors.yellow)),
  'amber': IrNativeValue(Value(Colors.amber)),
  'orange': IrNativeValue(Value(Colors.orange)),
  'deep-orange': IrNativeValue(Value(Colors.deepOrange)),
  'brown': IrNativeValue(Value(Colors.brown)),
  'grey': IrNativeValue(Value(Colors.grey)),
  'blue-grey': IrNativeValue(Value(Colors.blueGrey)),

  // Common shades
  'black': IrNativeValue(Value(Colors.black)),
  'white': IrNativeValue(Value(Colors.white)),
  'transparent': IrNativeValue(Value(Colors.transparent)),

  // Accent colors
  'red-accent': IrNativeValue(Value(Colors.redAccent)),
  'pink-accent': IrNativeValue(Value(Colors.pinkAccent)),
  'purple-accent': IrNativeValue(Value(Colors.purpleAccent)),
  'deep-purple-accent': IrNativeValue(Value(Colors.deepPurpleAccent)),
  'indigo-accent': IrNativeValue(Value(Colors.indigoAccent)),
  'blue-accent': IrNativeValue(Value(Colors.blueAccent)),
  'light-blue-accent': IrNativeValue(Value(Colors.lightBlueAccent)),
  'cyan-accent': IrNativeValue(Value(Colors.cyanAccent)),
  'teal-accent': IrNativeValue(Value(Colors.tealAccent)),
  'green-accent': IrNativeValue(Value(Colors.greenAccent)),
  'light-green-accent': IrNativeValue(Value(Colors.lightGreenAccent)),
  'lime-accent': IrNativeValue(Value(Colors.limeAccent)),
  'yellow-accent': IrNativeValue(Value(Colors.yellowAccent)),
  'amber-accent': IrNativeValue(Value(Colors.amberAccent)),
  'orange-accent': IrNativeValue(Value(Colors.orangeAccent)),
  'deep-orange-accent': IrNativeValue(Value(Colors.deepOrangeAccent)),
});
