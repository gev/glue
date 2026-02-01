import 'package:flutter/cupertino.dart';
import 'package:flutter/gestures.dart';
import 'package:flutter/services.dart';
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
      controller: properties.getValue<TextEditingController>('controller'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      decoration: properties.getValue<BoxDecoration>('decoration'),
      padding:
          properties.getValue<EdgeInsetsGeometry>('padding') ?? EdgeInsets.zero,
      placeholder: properties.getString('placeholder'),
      placeholderStyle: properties.getValue<TextStyle>('placeholder-style'),
      prefix: properties.getWidget('prefix'),
      prefixMode:
          properties.getValue<OverlayVisibilityMode>('prefix-mode') ??
          OverlayVisibilityMode.never,
      suffix: properties.getWidget('suffix'),
      suffixMode:
          properties.getValue<OverlayVisibilityMode>('suffix-mode') ??
          OverlayVisibilityMode.never,
      clearButtonMode:
          properties.getValue<OverlayVisibilityMode>('clear-button-mode') ??
          OverlayVisibilityMode.never,
      keyboardType: properties.getValue<TextInputType>('keyboard-type'),
      textInputAction: properties.getValue<TextInputAction>(
        'text-input-action',
      ),
      textCapitalization:
          properties.getValue<TextCapitalization>('text-capitalization') ??
          TextCapitalization.none,
      style: properties.getValue<TextStyle>('style'),
      strutStyle: properties.getValue<StrutStyle>('strut-style'),
      textAlign:
          properties.getValue<TextAlign>('text-align') ?? TextAlign.start,
      textAlignVertical: properties.getValue<TextAlignVertical>(
        'text-align-vertical',
      ),
      readOnly: properties.getBool('read-only') ?? false,
      showCursor: properties.getBool('show-cursor'),
      autofocus: properties.getBool('autofocus') ?? false,
      obscuringCharacter: properties.getString('obscuring-character') ?? '•',
      obscureText: properties.getBool('obscure-text') ?? false,
      autocorrect: properties.getBool('autocorrect') ?? true,
      smartDashesType: properties.getValue<SmartDashesType>(
        'smart-dashes-type',
      ),
      smartQuotesType: properties.getValue<SmartQuotesType>(
        'smart-quotes-type',
      ),
      enableSuggestions: properties.getBool('enable-suggestions') ?? true,
      maxLines: properties.getInt('max-lines'),
      minLines: properties.getInt('min-lines'),
      expands: properties.getBool('expands') ?? false,
      maxLength: properties.getInt('max-length'),
      maxLengthEnforcement: properties.getValue<MaxLengthEnforcement>(
        'max-length-enforcement',
      ),
      onChanged: properties.getValue<ValueChanged<String>>('on-changed'),
      onEditingComplete: properties
          .getVoidCallback('on-editing-complete')
          ?.call(runtime),
      onSubmitted: properties.getValue<ValueChanged<String>>('on-submitted'),
      inputFormatters: properties.getValue<List<TextInputFormatter>>(
        'input-formatters',
      ),
      enabled: properties.getBool('enabled') ?? true,
      cursorWidth: properties.getDouble('cursor-width') ?? 2.0,
      cursorHeight: properties.getDouble('cursor-height'),
      cursorRadius:
          properties.getValue<Radius>('cursor-radius') ?? Radius.circular(2.0),
      cursorColor: properties.getColor('cursor-color'),
      keyboardAppearance: properties.getValue<Brightness>(
        'keyboard-appearance',
      ),
      scrollPadding:
          properties.getValue<EdgeInsets>('scroll-padding') ??
          const EdgeInsets.all(20.0),
      dragStartBehavior:
          properties.getValue<DragStartBehavior>('drag-start-behavior') ??
          DragStartBehavior.start,
      enableInteractiveSelection:
          properties.getBool('enable-interactive-selection') ?? true,
      selectionControls: properties.getValue<TextSelectionControls>(
        'selection-controls',
      ),
      onTap: properties.getVoidCallback('on-tap')?.call(runtime),
      scrollController: properties.getValue<ScrollController>(
        'scroll-controller',
      ),
      scrollPhysics: properties.getValue<ScrollPhysics>('scroll-physics'),
      autofillHints: properties.getValue<List<String>>('autofill-hints'),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
      restorationId: properties.getString('restoration-id'),
      enableIMEPersonalizedLearning:
          properties.getBool('enable-ime-personalized-learning') ?? true,
    );
    return IrNativeValue(Value(widget));
  });
}
