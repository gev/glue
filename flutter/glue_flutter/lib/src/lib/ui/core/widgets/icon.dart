import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Icon widget function
/// Creates Flutter Icon from Glue (icon props) expressions
final Ir icon = IrNativeFunc(iconImpl);

/// Icon implementation - takes properties object
Eval<Ir> iconImpl(Ir props) => _createIcon(WidgetProperties(props));

/// Create Icon widget from properties
Eval<Ir> _createIcon(WidgetProperties properties) {
  final iconWidget = Icon(
    properties.getValue<IconData>('icon'),
    key: properties.key,
    size: properties.getDouble('size'),
    fill: properties.getDouble('fill'),
    weight: properties.getDouble('weight'),
    grade: properties.getDouble('grade'),
    opticalSize: properties.getDouble('optical-size'),
    color: properties.getColor('color'),
    shadows: properties.getValues<Shadow>('shadows'),
    semanticLabel: properties.getString('semantic-label'),
    textDirection: properties.getValue<TextDirection>('text-direction'),
    blendMode: properties.getValue<BlendMode>('blend-mode'),
    fontWeight: properties.getValue<FontWeight>('font-weight'),
  );
  return Eval.pure(IrNativeValue(Value(iconWidget)));
}
