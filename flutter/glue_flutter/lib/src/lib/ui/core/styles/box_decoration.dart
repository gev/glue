import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir boxDecoration = IrNativeFunc((props) {
  final properties = WidgetProperties(props);
  return Eval.pure(
    IrNativeValue(
      Value(
        BoxDecoration(
          color: properties.getValue<Color>('color'),
          image: properties.getValue<DecorationImage>('image'),
          border: properties.getValue<BoxBorder>('border'),
          borderRadius: properties.getValue<BorderRadiusGeometry>(
            'border-radius',
          ),
          boxShadow: properties.getValues<BoxShadow>('box-shadow'),
          gradient: properties.getValue<Gradient>('gradient'),
          backgroundBlendMode: properties.getValue<BlendMode>(
            'background-blend-mode',
          ),
          shape: properties.getValue<BoxShape>('shape') ?? BoxShape.rectangle,
        ),
      ),
    ),
  );
});
