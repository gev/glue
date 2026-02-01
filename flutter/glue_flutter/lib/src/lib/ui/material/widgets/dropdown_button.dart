import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// DropdownButton widget function
/// Creates Flutter DropdownButton from Glue (dropdown-button props) expressions
final Ir dropdownButton = IrNativeFunc(dropdownButtonImpl);

/// DropdownButton implementation - takes properties object
Eval<Ir> dropdownButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createDropdownButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createDropdownButton(WidgetProperties.empty()),
};

/// Create DropdownButton widget from properties
Eval<Ir> _createDropdownButton(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final dropdownButtonWidget = DropdownButton<Object>(
      key: properties.key,
      items: properties.getValue<>('items'),
      selectedItemBuilder: properties.getValue<>('selected-item-builder'),
      value: properties.getValue<>('value'),
      hint: properties.getWidget('hint'),
      disabledHint: properties.getWidget('disabled-hint'),
      onChanged: properties.getValue<>('on-changed'),
      onTap: properties.getVoidCallback('on-tap', runtime),
      elevation: properties.getInt('elevation') ?? 8,
      style: properties.getValue<>('style'),
      underline: properties.getWidget('underline'),
      icon: properties.getWidget('icon'),
      iconDisabledColor: properties.getColor('icon-disabled-color'),
      iconEnabledColor: properties.getColor('icon-enabled-color'),
      iconSize: properties.getDouble('icon-size') ?? 24.0,
      isDense: properties.getBool('is-dense') ?? false,
      isExpanded: properties.getBool('is-expanded') ?? false,
      itemHeight: properties.getDouble('item-height'),
      focusColor: properties.getColor('focus-color'),
      focusNode: properties.getValue<>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      dropdownColor: properties.getColor('color'),
      menuMaxHeight: properties.getDouble('menu-max-height'),
      enableFeedback: properties.getBool('enable-feedback') ?? true,
      alignment: properties.getValue<>('alignment'),
      borderRadius: properties.getValue<>('border-radius'),
      padding: properties.getValue<>('padding'),
    );
    return IrNativeValue(Value(dropdownButtonWidget));
  });
}
