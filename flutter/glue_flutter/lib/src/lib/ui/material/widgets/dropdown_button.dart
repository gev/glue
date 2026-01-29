import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// DropdownButton widget function
/// Creates Flutter DropdownButton from Glue (dropdown-button props) expressions
final Ir dropdownButton = IrNativeFunc(dropdownButtonImpl);

/// DropdownButton implementation - takes properties object
Eval<Ir> dropdownButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDropdownButton(
    MaterialProperties(properties.unlock),
  ),
  _ => _createDropdownButton(MaterialProperties.empty()),
};

/// Create DropdownButton widget from properties
Eval<Ir> _createDropdownButton(MaterialProperties properties) {
  final dropdownButtonWidget = DropdownButton<Object>(
    items: properties.dropdownItems,
    selectedItemBuilder: properties.dropdownSelectedItemBuilder,
    value: properties.dropdownValue,
    hint: properties.dropdownHint,
    disabledHint: properties.dropdownDisabledHint,
    onChanged: properties.dropdownOnChanged,
    onTap: properties.dropdownOnTap,
    elevation: properties.dropdownElevation,
    style: properties.dropdownStyle,
    underline: properties.dropdownUnderline,
    icon: properties.dropdownIcon,
    iconDisabledColor: properties.dropdownIconDisabledColor,
    iconEnabledColor: properties.dropdownIconEnabledColor,
    iconSize: properties.dropdownIconSize,
    isDense: properties.dropdownIsDense,
    isExpanded: properties.dropdownIsExpanded,
    itemHeight: properties.dropdownItemHeight,
    focusColor: properties.dropdownFocusColor,
    focusNode: properties.dropdownFocusNode,
    autofocus: properties.dropdownAutofocus,
    dropdownColor: properties.dropdownDropdownColor,
    menuMaxHeight: properties.dropdownMenuMaxHeight,
    enableFeedback: properties.dropdownEnableFeedback,
    alignment: properties.dropdownAlignment,
    borderRadius: properties.dropdownBorderRadius,
    padding: properties.dropdownPadding,
  );
  return Eval.pure(IrNativeValue(Value(dropdownButtonWidget)));
}
