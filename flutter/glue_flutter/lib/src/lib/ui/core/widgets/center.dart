import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center props) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes properties object
Eval<Ir> centerImpl(Ir props) => _createCenter(WidgetProperties(props));

/// Create Center widget from properties
Eval<Ir> _createCenter(WidgetProperties properties) {
  final centerWidget = Center(key: properties.key, child: properties.child);
  return Eval.pure(IrNativeValue(Value(centerWidget)));
}
