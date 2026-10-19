import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// FittedBox widget function
/// Creates Flutter FittedBox from Glue (fitted-box props) expressions
final Ir fittedBox = IrNativeFunc(fittedBoxImpl);

/// FittedBox implementation - takes properties object
Eval<Ir> fittedBoxImpl(Ir props) => _createFittedBox(WidgetProperties(props));

/// Create FittedBox widget from properties
Eval<Ir> _createFittedBox(WidgetProperties properties) {
  final fittedBoxWidget = FittedBox(
    key: properties.key,
    fit: properties.getValue<BoxFit>('fit') ?? BoxFit.contain,
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ?? Alignment.center,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(fittedBoxWidget)));
}
