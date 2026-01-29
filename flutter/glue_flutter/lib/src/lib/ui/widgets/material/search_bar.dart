import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// SearchBar widget function
/// Creates Flutter SearchBar from Glue (search-bar props) expressions
final Ir searchBar = IrNativeFunc(searchBarImpl);

/// SearchBar implementation - takes properties object
Eval<Ir> searchBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSearchBar(
    Properties(properties.unlock),
  ),
  _ => _createSearchBar(Properties.empty()),
};

/// Create SearchBar widget from properties
Eval<Ir> _createSearchBar(Properties properties) {
  final searchBarWidget = SearchBar(
    controller: properties.searchBarController,
    focusNode: properties.searchBarFocusNode,
    hintText: properties.searchBarHintText,
    leading: properties.searchBarLeading,
    trailing: properties.searchBarTrailing,
    onTap: properties.searchBarOnTap,
    onChanged: properties.searchBarOnChanged,
    onSubmitted: properties.searchBarOnSubmitted,
    constraints: properties.searchBarConstraints,
    elevation: properties.searchBarElevation,
    backgroundColor: properties.searchBarBackgroundColor,
    shadowColor: properties.searchBarShadowColor,
    surfaceTintColor: properties.searchBarSurfaceTintColor,
    overlayColor: properties.searchBarOverlayColor,
    side: properties.searchBarSide,
    shape: properties.searchBarShape,
    padding: WidgetStateProperty.all(properties.searchBarPadding),
    textStyle: WidgetStateProperty.all(properties.searchBarTextStyle),
    hintStyle: WidgetStateProperty.all(properties.searchBarHintStyle),
    textCapitalization: properties.searchBarTextCapitalization,
    keyboardType: properties.searchBarKeyboardType,
    onTapOutside: properties.searchBarOnTapOutside,
  );
  return Eval.pure(IrNativeValue(Value(searchBarWidget)));
}
