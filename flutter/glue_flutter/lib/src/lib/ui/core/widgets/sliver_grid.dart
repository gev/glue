import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SliverGrid widget function
/// Creates Flutter SliverGrid from Glue (sliver-grid props) expressions
final Ir sliverGrid = IrNativeFunc(sliverGridImpl);

/// SliverGrid implementation - takes properties object
Eval<Ir> sliverGridImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSliverGrid(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSliverGrid(WidgetProperties.empty()),
};

/// Create SliverGrid widget from properties
Eval<Ir> _createSliverGrid(WidgetProperties properties) {
  final delegate = properties.getValue<SliverChildDelegate>('delegate');
  if (delegate == null) {
    return throwError(wrongArgumentType(['delegate property required']));
  }
  final gridDelegate = properties.getValue<SliverGridDelegate>('grid-delegate');
  if (gridDelegate == null) {
    return throwError(wrongArgumentType(['grid-delegate property required']));
  }
  final sliverGridWidget = SliverGrid(
    key: properties.key,
    delegate: delegate,
    gridDelegate: gridDelegate,
  );
  return Eval.pure(IrNativeValue(Value(sliverGridWidget)));
}
