import 'dart:ui';
import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BackdropFilter widget function
/// Creates Flutter BackdropFilter from Glue (backdrop-filter props) expressions
final Ir backdropFilter = IrNativeFunc((Ir props) {
  switch (props) {
    case IrObject(:final properties):
      final props = WidgetProperties(properties.unlock);
      final backdropFilterWidget = BackdropFilter(
        key: props.key,
        filter: props.getValue<ImageFilter>('filter') ?? ImageFilter.blur(),
        filterConfig: props.getValue<ImageFilterConfig>('filter-config'),
        blendMode: props.getValue<BlendMode>('blend-mode') ?? BlendMode.srcOver,
        enabled: props.getBool('enabled') ?? true,
        child: props.child,
      );
      return Eval.pure(IrNativeValue(Value(backdropFilterWidget)));
    default:
      return throwError(wrongArgumentType(['object']));
  }
});
