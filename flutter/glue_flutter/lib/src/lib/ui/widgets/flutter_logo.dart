import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// FlutterLogo widget function
/// Creates Flutter FlutterLogo from Glue (flutter-logo props) expressions
final Ir flutterLogo = IrNativeFunc(flutterLogoImpl);

/// FlutterLogo implementation - takes properties object
Eval<Ir> flutterLogoImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFlutterLogo(
    Properties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create FlutterLogo widget from properties
Eval<Ir> _createFlutterLogo(Properties properties) {
  final flutterLogoWidget = FlutterLogo(
    size: properties.size ?? 100.0,
    style: properties.flutterLogoStyle ?? FlutterLogoStyle.markOnly,
    textColor: properties.color ?? const Color(0xFF757575),
    duration: properties.duration ?? const Duration(milliseconds: 750),
    curve: properties.curve ?? Curves.fastOutSlowIn,
  );
  return Eval.pure(IrNativeValue(Value(flutterLogoWidget)));
}
