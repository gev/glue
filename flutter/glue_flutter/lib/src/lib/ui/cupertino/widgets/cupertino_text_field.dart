import 'package:flutter/cupertino.dart';
import 'package:flutter/gestures.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTextField widget function
/// Creates Flutter CupertinoTextField from Glue expressions
/// Expects keyword arguments: :controller, :placeholder, :on-changed, etc.
final Ir cupertinoTextField = IrNativeFunc(cupertinoTextFieldImpl);

/// CupertinoTextField implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTextFieldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTextField(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoTextField(WidgetProperties.empty()),
};

/// Create CupertinoTextField widget from properties object
Eval<Ir> _createCupertinoTextField(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoTextField(
      key: properties.key,
      controller: properties.getValue('controller'),
      focusNode: properties.getValue('focus-node'),
      decoration: properties.getValue('decoration'),
      padding: properties.getValue('padding'),
      placeholder: properties.getString('placeholder'),
      placeholderStyle: properties.getValue('placeholder-style'),
      prefix: properties.getWidget('prefix'),
      prefixMode: properties.getValue('prefix-mode'),
      suffix: properties.getWidget('suffix'),
      suffixMode: properties.getValue('suffix-mode'),
      clearButtonMode: properties.getValue('clear-button-mode'),
      keyboardType: properties.getValue('keyboard-type'),
      textInputAction: properties.getValue('text-input-action'),
      textCapitalization: properties.getValue('text-capitalization'),
      style: properties.getValue('style'),
      strutStyle: properties.getValue('strut-style'),
      textAlign: properties.getValue('text-align'),
      textAlignVertical: properties.getValue('text-align-vertical'),
      readOnly: properties.getBool('read-only') ?? false,
      showCursor: properties.getBool('show-cursor'),
      autofocus: properties.getBool('autofocus') ?? false,
      obscuringCharacter: properties.getString('obscuring-character') ?? '•',
      obscureText: properties.getBool('obscure-text') ?? false,
      autocorrect: properties.getBool('autocorrect') ?? true,
      smartDashesType: properties.getValue('smart-dashes-type'),
      smartQuotesType: properties.getValue('smart-quotes-type'),
      enableSuggestions: properties.getBool('enable-suggestions') ?? true,
      maxLines: properties.getInt('max-lines'),
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
      cursorHeight: properties.getDouble('cursor-height'),
      cursorRadius: properties.getValue('cursor-radius'),
      cursorColor: properties.getColor('cursor-color'),
      keyboardAppearance: properties.getValue('keyboard-appearance'),
      scrollPadding:
          properties.getValue('scroll-padding') ?? const EdgeInsets.all(20.0),
      dragStartBehavior:
          properties.getValue('drag-start-behavior') ?? DragStartBehavior.start,
      enableInteractiveSelection:
          properties.getBool('enable-interactive-selection') ?? true,
      selectionControls: properties.getValue('selection-controls'),
      onTap: properties.getVoidCallback('on-tap', runtime),
      scrollController: properties.getValue('scroll-controller'),
      scrollPhysics: properties.getValue('scroll-physics'),
      autofillHints: properties.getValue('autofill-hints'),
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.hardEdge,
      restorationId: properties.getString('restoration-id'),
      enableIMEPersonalizedLearning:
          properties.getBool('enable-ime-personalized-learning') ?? true,
    );
    return IrNativeValue(Value(widget));
  });
}
