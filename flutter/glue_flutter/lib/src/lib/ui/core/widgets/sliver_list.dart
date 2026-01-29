import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SliverList widget function
/// Creates Flutter SliverList from Glue (sliver-list props) expressions
final Ir sliverList = IrNativeFunc(sliverListImpl);

/// SliverList implementation - takes properties object
Eval<Ir> sliverListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSliverList(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSliverList(WidgetProperties.empty()),
};

/// Create SliverList widget from properties
Eval<Ir> _createSliverList(WidgetProperties properties) {
  final sliverListWidget = SliverList(
    key: properties.key,
    delegate:
        properties.getValue('sliver-list-delegate') ??
        SliverChildListDelegate([]),
  );
  return Eval.pure(IrNativeValue(Value(sliverListWidget)));
}
