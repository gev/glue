import 'dart:ui';
import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BackdropFilter widget function
/// Creates Flutter BackdropFilter from Glue (backdrop-filter props) expressions
final Ir backdropFilter = IrNativeFunc((Ir props) {
  final properties = WidgetProperties(props);
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
});
