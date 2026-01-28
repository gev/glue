import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SliverList widget function
/// Creates Flutter SliverList from Glue (sliver-list props) expressions
final Ir sliverList = IrNativeFunc(sliverListImpl);

/// SliverList implementation - takes properties object
Eval<Ir> sliverListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSliverList(
    Properties(properties.unlock),
  ),
  _ => _createSliverList(Properties.empty()),
};

/// Create SliverList widget from properties
Eval<Ir> _createSliverList(Properties properties) {
  final sliverListWidget = SliverList(
    delegate: properties.sliverListDelegate ?? SliverChildListDelegate([]),
  );
  return Eval.pure(IrNativeValue(Value(sliverListWidget)));
}
