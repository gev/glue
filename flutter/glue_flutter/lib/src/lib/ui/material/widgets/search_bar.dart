import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SearchBar widget function
/// Creates Flutter SearchBar from Glue (search-bar props) expressions
final Ir searchBar = IrNativeFunc(searchBarImpl);

/// SearchBar implementation - takes properties object
Eval<Ir> searchBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSearchBar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSearchBar(WidgetProperties.empty()),
};

/// Create SearchBar widget from properties
Eval<Ir> _createSearchBar(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final searchBarWidget = SearchBar(
      key: properties.key,
      controller: properties.getValue('controller'),
      focusNode: properties.getValue('focus-node'),
      hintText: properties.getString('hint-text'),
      onTap: properties.getVoidCallback('on-tap', runtime),
      onChanged: properties.getValue('on-changed'),
      onSubmitted: properties.getValue('on-submitted'),
      constraints: properties.getValue('constraints'),
      elevation: properties.getValue('elevation'),
      overlayColor: properties.getValue('overlay-color'),
      side: properties.getValue('side'),
      shape: properties.getValue('shape'),
      padding: WidgetStateProperty.all(
        properties.getValue('padding') ??
            const EdgeInsets.symmetric(horizontal: 8.0),
      ),
      textStyle: WidgetStateProperty.all(properties.getValue('text-style')),
      hintStyle: WidgetStateProperty.all(properties.getValue('hint-style')),
      textCapitalization:
          properties.getValue('text-capitalization') ?? TextCapitalization.none,
      keyboardType: properties.getValue('keyboard-type') ?? TextInputType.text,
      // Note: Some SearchBar parameters have API compatibility issues
      // (leading/trailing widgets, color properties, onTapOutside callback)
      // that will be resolved in future Flutter version updates
    );
    return IrNativeValue(Value(searchBarWidget));
  });
}
