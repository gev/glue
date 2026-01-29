import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// SearchAnchor widget function
/// Creates Flutter SearchAnchor from Glue (search-anchor props) expressions
final Ir searchAnchor = IrNativeFunc(searchAnchorImpl);

/// SearchAnchor implementation - takes properties object
Eval<Ir> searchAnchorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSearchAnchor(
    MaterialProperties(properties.unlock),
  ),
  _ => _createSearchAnchor(MaterialProperties.empty()),
};

/// Create SearchAnchor widget from properties
Eval<Ir> _createSearchAnchor(MaterialProperties properties) {
  final searchAnchorWidget = SearchAnchor(
    searchController: properties.searchAnchorSearchController,
    suggestionsBuilder: (context, controller) => [],
    builder: (context, controller) => const SizedBox(),
    viewConstraints: properties.searchAnchorViewConstraints,
    viewElevation: properties.searchAnchorViewElevation,
    viewBackgroundColor: properties.searchAnchorViewBackgroundColor,
    viewSurfaceTintColor: properties.searchAnchorViewSurfaceTintColor,
    viewShape: properties.searchAnchorViewShape,
    viewSide: properties.searchAnchorViewSide,
    viewPadding: properties.searchAnchorViewPadding,
    viewLeading: properties.searchAnchorViewLeading,
    viewTrailing: properties.searchAnchorViewTrailing,
    viewHintText: properties.searchAnchorViewHintText,
    isFullScreen: properties.searchAnchorIsFullScreen,
    dividerColor: properties.searchAnchorDividerColor,
  );
  return Eval.pure(IrNativeValue(Value(searchAnchorWidget)));
}
