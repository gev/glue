import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FlutterLogo widget function
/// Creates Flutter FlutterLogo from Glue (flutter-logo props) expressions
final Ir flutterLogo = IrNativeFunc(flutterLogoImpl);

/// FlutterLogo implementation - takes properties object
Eval<Ir> flutterLogoImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createFlutterLogo(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create FlutterLogo widget from properties
Eval<Ir> _createFlutterLogo(WidgetProperties properties) {
  final flutterLogoWidget = FlutterLogo(
    key: properties.key,
    size: properties.getDouble('size'),
    style: properties.getValue('style') ?? FlutterLogoStyle.markOnly,
    textColor: properties.getColor('text-color') ?? const Color(0xFF616161),
    duration:
        properties.getValue('duration') ?? const Duration(milliseconds: 750),
    curve: properties.getValue('curve') ?? Curves.fastOutSlowIn,
  );
  return Eval.pure(IrNativeValue(Value(flutterLogoWidget)));
}
