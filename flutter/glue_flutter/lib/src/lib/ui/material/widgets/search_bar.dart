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
      controller: properties.getValue<TextEditingController>('controller'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      hintText: properties.getString('hint-text'),
      onTap: properties.getVoidCallback('on-tap')?.call(runtime),
      onChanged: properties.getValue<ValueChanged<String>>('on-changed'),
      onSubmitted: properties.getValue<ValueChanged<String>>('on-submitted'),
      constraints: properties.getValue<BoxConstraints>('constraints'),
      elevation: properties.getValue<WidgetStateProperty<double?>>('elevation'),
      overlayColor: properties.getValue<WidgetStateProperty<Color?>>(
        'overlay-color',
      ),
      side: properties.getValue<WidgetStateProperty<BorderSide?>>('side'),
      shape: properties.getValue<WidgetStateProperty<OutlinedBorder?>>('shape'),
      padding: properties.getValue<WidgetStateProperty<EdgeInsetsGeometry?>>(
        'padding',
      ),
      textStyle: properties.getValue<WidgetStateProperty<TextStyle?>>(
        'text-style',
      ),
      hintStyle: properties.getValue<WidgetStateProperty<TextStyle?>>(
        'hint-style',
      ),
      textCapitalization:
          properties.getValue<TextCapitalization>('text-capitalization') ??
          TextCapitalization.none,
      keyboardType:
          properties.getValue<TextInputType>('keyboard-type') ??
          TextInputType.text,
    );
    return IrNativeValue(Value(searchBarWidget));
  });
}
