import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir boxShadow = IrNativeFunc((props) {
  final properties = WidgetProperties(props);
  return Eval.pure(
    IrNativeValue(
      Value(
        BoxShadow(
          color: properties.getValue<Color>('color') ?? const Color(0xFF000000),
          offset: properties.getValue<Offset>('offset') ?? Offset.zero,
          blurRadius: properties.getDouble('blur-radius') ?? 0.0,
          spreadRadius: properties.getDouble('spread-radius') ?? 0.0,
          blurStyle:
              properties.getValue<BlurStyle>('blur-style') ?? BlurStyle.normal,
        ),
      ),
    ),
  );
});
