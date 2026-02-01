import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSearchTextField widget function
/// Creates Flutter CupertinoSearchTextField from Glue expressions
/// Expects keyword arguments: :controller, :on-changed, :on-submitted, :hint-text, etc.
final Ir cupertinoSearchTextField = IrNativeFunc(cupertinoSearchTextFieldImpl);

/// CupertinoSearchTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSearchTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSearchTextField(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSearchTextField(WidgetProperties.empty()),
};

/// Create CupertinoSearchTextField widget from properties object
Eval<Ir> _createCupertinoSearchTextField(WidgetProperties properties) =>
    getRuntime().map((runtime) {
      final widget = CupertinoSearchTextField(
        key: properties.key,
        controller: properties.getValue<>('controller'),
        onChanged: properties.getValue<>('on-changed'),
        onSubmitted: properties.getValue<>('on-submitted'),
        style: properties.getValue<>('style'),
        placeholder: properties.getString('placeholder'),
        placeholderStyle: properties.getValue<>('placeholder-style'),
        decoration: properties.getValue<>('decoration'),
        backgroundColor: properties.getColor('background-color'),
        borderRadius: properties.getValue<>('border-radius'),
        padding: properties.getValue<>('padding'),
        itemColor:
            properties.getColor('item-color') ?? CupertinoColors.secondaryLabel,
        itemSize: properties.getDouble('item-size') ?? 20,
        prefixIcon:
            properties.getWidget('prefix-icon') ?? Icon(CupertinoIcons.search),
        suffixIcon:
            properties.getValue<>('suffix-icon') ??
            Icon(CupertinoIcons.xmark_circle_fill),
        suffixMode: properties.getValue<>('suffix-mode'),
        onSuffixTap: properties.getVoidCallback('on-suffix-tap', runtime),
        enabled: properties.getBool('enabled'),
        autocorrect: properties.getBool('autocorrect') ?? true,
        focusNode: properties.getValue<>('focus-node'),
        autofocus: properties.getBool('autofocus') ?? false,
      );
      return IrNativeValue(Value(widget));
    });
