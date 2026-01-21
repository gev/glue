import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

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

/// RGB function - creates color from list [r,g,b] values (0-255)
final rgb = IrNativeFunc(rgbImpl);

/// RGBA function - creates color from list [r,g,b,a] values (0-255)
final rgba = IrNativeFunc(rgbaImpl);

Eval<Ir> rgbImpl(Ir args) => switch (args) {
  IrList(elements: final elements) => createRgbFromList(elements.unlock),
  _ => throwError(wrongArgumentType(['list'])),
};

Eval<Ir> rgbaImpl(Ir args) => switch (args) {
  IrList(elements: final elements) => createRgbaFromList(elements.unlock),
  _ => throwError(wrongArgumentType(['list'])),
};

/// Create RGB color from list [r, g, b]
Eval<Ir> createRgbFromList(List<Ir> elements) {
  if (elements.length != 3) {
    return throwError(
      RuntimeException(
        'rgb-error',
        IrString('RGB requires exactly 3 values [r, g, b]'),
      ),
    );
  }
  return createRgbColor(elements[0], elements[1], elements[2]);
}

/// Create RGBA color from list [r, g, b, a]
Eval<Ir> createRgbaFromList(List<Ir> elements) {
  if (elements.length != 4) {
    return throwError(
      RuntimeException(
        'rgba-error',
        IrString('RGBA requires exactly 4 values [r, g, b, a]'),
      ),
    );
  }
  return createRgbaColor(elements[0], elements[1], elements[2], elements[3]);
}

Eval<Ir> createRgbColor(Ir r, Ir g, Ir b) {
  final red = extractInt(r)?.clamp(0, 255) ?? 0;
  final green = extractInt(g)?.clamp(0, 255) ?? 0;
  final blue = extractInt(b)?.clamp(0, 255) ?? 0;
  final color = Color.fromARGB(255, red, green, blue);
  return Eval.pure(IrNativeValue(HostValue(color)));
}

Eval<Ir> createRgbaColor(Ir r, Ir g, Ir b, Ir a) {
  final red = extractInt(r)?.clamp(0, 255) ?? 0;
  final green = extractInt(g)?.clamp(0, 255) ?? 0;
  final blue = extractInt(b)?.clamp(0, 255) ?? 0;
  final alpha = extractInt(a)?.clamp(0, 255) ?? 255;
  final color = Color.fromARGB(alpha, red, green, blue);
  return Eval.pure(IrNativeValue(HostValue(color)));
}
