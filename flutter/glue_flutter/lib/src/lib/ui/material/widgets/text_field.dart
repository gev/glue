import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TextField widget function
/// Creates Flutter TextField from Glue (text-field props) expressions
final Ir textField = IrNativeFunc(textFieldImpl);

/// TextField implementation - takes properties object
Eval<Ir> textFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createTextField(
    WidgetProperties(properties.unlock),
  ),
  _ => _createTextField(WidgetProperties.empty()),
};

/// Create TextField widget from properties
Eval<Ir> _createTextField(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final textFieldWidget = TextField(
      key: properties.key,
      controller: properties.getValue('controller'),
      focusNode: properties.getValue('focus-node'),
      decoration: properties.getValue('decoration') ?? const InputDecoration(),
      keyboardType: properties.getValue('keyboard-type'),
      textInputAction: properties.getValue('text-input-action'),
      textCapitalization: properties.getValue('text-capitalization'),
      style: properties.getValue('style'),
      textAlign: properties.getValue('align') ?? TextAlign.start,
      textAlignVertical: properties.getValue('text-align-vertical'),
      textDirection: properties.getValue('text-direction'),
      readOnly: properties.getBool('read-only') ?? false,
      autofocus: properties.getBool('autofocus') ?? false,
      obscuringCharacter: properties.getString('obscuring-character') ?? '•',
      obscureText: properties.getBool('obscure-text') ?? false,
      autocorrect: true, // default to true
      smartDashesType: SmartDashesType.enabled,
      smartQuotesType: SmartQuotesType.enabled,
      enableSuggestions: properties.getBool('enable-suggestions') ?? true,
      maxLines: properties.getInt('max-lines') ?? 1,
      minLines: properties.getInt('min-lines'),
      expands: properties.getBool('expands') ?? false,
      maxLength: properties.getInt('max-length'),
      maxLengthEnforcement: properties.getValue('max-length-enforcement'),
      onChanged: properties.getValue('on-changed'),
      onEditingComplete: properties.getVoidCallback(
        'on-editing-complete',
        runtime,
      ),
      onSubmitted: properties.getValue('on-submitted'),
      inputFormatters: properties.getValue('input-formatters'),
      enabled: properties.getBool('enabled') ?? true,
      cursorWidth: properties.getDouble('cursor-width') ?? 2.0,
      cursorHeight: properties.getDouble('cursor-height') ?? 16.0,
      cursorRadius: properties.getValue('cursor-radius'),
      cursorColor: properties.getColor('cursor-color'),
      cursorErrorColor: properties.getColor('cursor-error-color'),
      keyboardAppearance: properties.getValue('keyboard-appearance'),
      scrollPadding: properties.getValue('scroll-padding'),
      enableInteractiveSelection:
          properties.getBool('enable-interactive-selection') ?? true,
      selectAllOnFocus: properties.getBool('select-all-on-focus') ?? false,
      selectionControls: properties.getValue('selection-controls'),
      onTap: properties.getVoidCallback('on-tap', runtime),
      onTapAlwaysCalled: properties.getBool('on-tap-always-called') ?? false,
      mouseCursor: properties.getValue('mouse-cursor'),
      scrollController: properties.getValue('scroll-controller'),
      scrollPhysics: properties.getValue('scroll-physics'),
      autofillHints: properties.getValue('autofill-hints'),
      clipBehavior: properties.getValue('clip-behavior'),
      restorationId: properties.getString('restoration-id'),
    );
    return IrNativeValue(Value(textFieldWidget));
  });
}
