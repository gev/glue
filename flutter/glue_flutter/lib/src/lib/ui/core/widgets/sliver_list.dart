import 'package:flutter/widgets.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// SliverList widget function
/// Creates Flutter SliverList from Glue (sliver-list props) expressions
final Ir sliverList = IrNativeFunc(sliverListImpl);

/// SliverList implementation - takes properties object
Eval<Ir> sliverListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSliverList(
    CoreProperties(properties.unlock),
  ),
  _ => _createSliverList(CoreProperties.empty()),
};

/// Create SliverList widget from properties
Eval<Ir> _createSliverList(CoreProperties properties) {
  final sliverListWidget = SliverList(
    delegate: properties.sliverListDelegate ?? SliverChildListDelegate([]),
  );
  return Eval.pure(IrNativeValue(Value(sliverListWidget)));
}
