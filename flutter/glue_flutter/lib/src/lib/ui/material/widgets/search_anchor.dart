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
    viewConstraints: properties.getValue('view-constraints'),
    viewElevation: properties.getDouble('view-elevation'),
    viewBackgroundColor: properties.getColor('view-background-color'),
    viewSurfaceTintColor: properties.getColor('view-surface-tint-color'),
    viewShape: properties.getValue('view-shape'),
    viewSide: properties.getValue('view-side'),
    viewPadding: properties.getValue('view-padding'),
    viewLeading: properties.getWidgets('view-leading'),
    viewTrailing: properties.getWidgets('view-trailing'),
    viewHintText: properties.getString('view-hint-text'),
    isFullScreen: properties.getBool('is-full-screen') ?? false,
    dividerColor: properties.getColor('divider-color'),
  );
  return Eval.pure(IrNativeValue(Value(searchAnchorWidget)));
}
