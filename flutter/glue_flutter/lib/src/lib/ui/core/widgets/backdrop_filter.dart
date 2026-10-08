import 'dart:ui';
import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BackdropFilter widget function
/// Creates Flutter BackdropFilter from Glue (backdrop-filter props) expressions
final Ir backdropFilter = IrNativeFunc(backdropFilterImpl);

/// BackdropFilter implementation - takes properties object
Eval<Ir> backdropFilterImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBackdropFilter(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create BackdropFilter widget from properties
Eval<Ir> _createBackdropFilter(WidgetProperties properties) {
  final backdropFilterWidget = BackdropFilter(
    key: properties.key,
    filter: properties.getValue<ImageFilter>('filter') ?? ImageFilter.blur(),
    filterConfig: properties.getValue<ImageFilterConfig>('filter-config'),
    blendMode:
        properties.getValue<BlendMode>('blend-mode') ?? BlendMode.srcOver,
    enabled: properties.getBool('enabled') ?? true,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(backdropFilterWidget)));
}
