import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SearchAnchor widget function
/// Creates Flutter SearchAnchor from Glue (search-anchor props) expressions
final Ir searchAnchor = IrNativeFunc(searchAnchorImpl);

/// SearchAnchor implementation - takes properties object
Eval<Ir> searchAnchorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSearchAnchor(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSearchAnchor(WidgetProperties.empty()),
};

/// Create SearchAnchor widget from properties
Eval<Ir> _createSearchAnchor(WidgetProperties properties) {
  final searchAnchorWidget = SearchAnchor(
    key: properties.key,
    searchController: properties.getValue('search-controller'),
    suggestionsBuilder:
        properties.getValue('suggestions-builder') ??
        (context, controller) => [],
    builder:
        properties.getValue('builder') ??
        (context, controller) => const SizedBox(),
    viewHintText: properties.getString('view-hint-text'),
    // Note: Some SearchAnchor parameters have API compatibility issues
    // that will be resolved in future Flutter version updates
  );
  return Eval.pure(IrNativeValue(Value(searchAnchorWidget)));
}
