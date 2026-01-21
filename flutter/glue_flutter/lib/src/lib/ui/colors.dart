import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Colors object - represents common named colors as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Color constant
final colors = IrObject({
  // Material Design colors
  'red': IrNativeValue(HostValue(Colors.red)),
  'pink': IrNativeValue(HostValue(Colors.pink)),
  'purple': IrNativeValue(HostValue(Colors.purple)),
  'deepPurple': IrNativeValue(HostValue(Colors.deepPurple)),
  'indigo': IrNativeValue(HostValue(Colors.indigo)),
  'blue': IrNativeValue(HostValue(Colors.blue)),
  'lightBlue': IrNativeValue(HostValue(Colors.lightBlue)),
  'cyan': IrNativeValue(HostValue(Colors.cyan)),
  'teal': IrNativeValue(HostValue(Colors.teal)),
  'green': IrNativeValue(HostValue(Colors.green)),
  'lightGreen': IrNativeValue(HostValue(Colors.lightGreen)),
  'lime': IrNativeValue(HostValue(Colors.lime)),
  'yellow': IrNativeValue(HostValue(Colors.yellow)),
  'amber': IrNativeValue(HostValue(Colors.amber)),
  'orange': IrNativeValue(HostValue(Colors.orange)),
  'deepOrange': IrNativeValue(HostValue(Colors.deepOrange)),
  'brown': IrNativeValue(HostValue(Colors.brown)),
  'grey': IrNativeValue(HostValue(Colors.grey)),
  'blueGrey': IrNativeValue(HostValue(Colors.blueGrey)),

  // Common shades
  'black': IrNativeValue(HostValue(Colors.black)),
  'white': IrNativeValue(HostValue(Colors.white)),
  'transparent': IrNativeValue(HostValue(Colors.transparent)),

  // Accent colors
  'redAccent': IrNativeValue(HostValue(Colors.redAccent)),
  'pinkAccent': IrNativeValue(HostValue(Colors.pinkAccent)),
  'purpleAccent': IrNativeValue(HostValue(Colors.purpleAccent)),
  'deepPurpleAccent': IrNativeValue(HostValue(Colors.deepPurpleAccent)),
  'indigoAccent': IrNativeValue(HostValue(Colors.indigoAccent)),
  'blueAccent': IrNativeValue(HostValue(Colors.blueAccent)),
  'lightBlueAccent': IrNativeValue(HostValue(Colors.lightBlueAccent)),
  'cyanAccent': IrNativeValue(HostValue(Colors.cyanAccent)),
  'tealAccent': IrNativeValue(HostValue(Colors.tealAccent)),
  'greenAccent': IrNativeValue(HostValue(Colors.greenAccent)),
  'lightGreenAccent': IrNativeValue(HostValue(Colors.lightGreenAccent)),
  'limeAccent': IrNativeValue(HostValue(Colors.limeAccent)),
  'yellowAccent': IrNativeValue(HostValue(Colors.yellowAccent)),
  'amberAccent': IrNativeValue(HostValue(Colors.amberAccent)),
  'orangeAccent': IrNativeValue(HostValue(Colors.orangeAccent)),
  'deepOrangeAccent': IrNativeValue(HostValue(Colors.deepOrangeAccent)),
});
