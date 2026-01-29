import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// SliverGrid widget function
/// Creates Flutter SliverGrid from Glue (sliver-grid props) expressions
final Ir sliverGrid = IrNativeFunc(sliverGridImpl);

/// SliverGrid implementation - takes properties object
Eval<Ir> sliverGridImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSliverGrid(
    Properties(properties.unlock),
  ),
  _ => _createSliverGrid(Properties.empty()),
};

/// Create SliverGrid widget from properties
Eval<Ir> _createSliverGrid(Properties properties) {
  final sliverGridWidget = SliverGrid(
    delegate: properties.sliverGridDelegate ?? SliverChildListDelegate([]),
    gridDelegate:
        properties.sliverGridGridDelegate ??
        const SliverGridDelegateWithFixedCrossAxisCount(crossAxisCount: 2),
  );
  return Eval.pure(IrNativeValue(Value(sliverGridWidget)));
}
