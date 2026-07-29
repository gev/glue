import 'package:flutter/material.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/color.dart';

/// Colors object - represents common named colors as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Color constant
final colors = IrObject({
  // Material Design colors
  'red': makeColor(Colors.red),
  'pink': makeColor(Colors.pink),
  'purple': makeColor(Colors.purple),
  'deep-purple': makeColor(Colors.deepPurple),
  'indigo': makeColor(Colors.indigo),
  'blue': makeColor(Colors.blue),
  'light-blue': makeColor(Colors.lightBlue),
  'cyan': makeColor(Colors.cyan),
  'teal': makeColor(Colors.teal),
  'green': makeColor(Colors.green),
  'light-green': makeColor(Colors.lightGreen),
  'lime': makeColor(Colors.lime),
  'yellow': makeColor(Colors.yellow),
  'amber': makeColor(Colors.amber),
  'orange': makeColor(Colors.orange),
  'deep-orange': makeColor(Colors.deepOrange),
  'brown': makeColor(Colors.brown),
  'grey': makeColor(Colors.grey),
  'blue-grey': makeColor(Colors.blueGrey),

  // Common shades
  'black': makeColor(Colors.black),
  'white': makeColor(Colors.white),
  'transparent': makeColor(Colors.transparent),

  // Accent colors
  'red-accent': makeColor(Colors.redAccent),
  'pink-accent': makeColor(Colors.pinkAccent),
  'purple-accent': makeColor(Colors.purpleAccent),
  'deep-purple-accent': makeColor(Colors.deepPurpleAccent),
  'indigo-accent': makeColor(Colors.indigoAccent),
  'blue-accent': makeColor(Colors.blueAccent),
  'light-blue-accent': makeColor(Colors.lightBlueAccent),
  'cyan-accent': makeColor(Colors.cyanAccent),
  'teal-accent': makeColor(Colors.tealAccent),
  'green-accent': makeColor(Colors.greenAccent),
  'light-green-accent': makeColor(Colors.lightGreenAccent),
  'lime-accent': makeColor(Colors.limeAccent),
  'yellow-accent': makeColor(Colors.yellowAccent),
  'amber-accent': makeColor(Colors.amberAccent),
  'orange-accent': makeColor(Colors.orangeAccent),
  'deep-orange-accent': makeColor(Colors.deepOrangeAccent),
});
