import 'dart:ui';

import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
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
      groupId: properties.getValue<Object>('group-id') ?? EditableText,
      controller: properties.getValue<TextEditingController>('controller'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      undoController: properties.getValue<UndoHistoryController>(
        'undo-controller',
      ),
      decoration: properties.getValue<InputDecoration>('decoration'),
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
      textDirection: properties.getValue<TextDirection>('text-direction'),
      readOnly: properties.getBool('read-only') ?? false,
      showCursor: properties.getBool('show-cursor'),
      autofocus: properties.getBool('autofocus') ?? false,
      statesController: properties.getValue<WidgetStatesController>(
        'states-controller',
      ),
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
      maxLines: properties.getInt('max-lines') ?? 1,
      minLines: properties.getInt('min-lines'),
      expands: properties.getBool('expands') ?? false,
      maxLength: properties.getInt('max-length'),
      maxLengthEnforcement: properties.getValue<MaxLengthEnforcement>(
        'max-length-enforcement',
      ),
      onChanged: properties.getCallback<String>('on-changed')?.call(runtime),
      onEditingComplete: properties
          .getVoidCallback('on-editing-complete')
          ?.call(runtime),
      onSubmitted: properties
          .getCallback<String>('on-submitted')
          ?.call(runtime),
      inputFormatters: properties.getValue<List<TextInputFormatter>>(
        'input-formatters',
      ),
      enabled: properties.getBool('enabled') ?? true,
      ignorePointers: properties.getBool('ignore-pointers'),
      cursorWidth: properties.getDouble('cursor-width') ?? 2.0,
      cursorHeight: properties.getDouble('cursor-height') ?? 16.0,
      cursorRadius: properties.getValue<Radius>('cursor-radius'),
      cursorOpacityAnimates: properties.getBool('cursor-opacity-animates'),
      cursorColor: properties.getColor('cursor-color'),
      cursorErrorColor: properties.getColor('cursor-error-color'),
      selectionHeightStyle: properties.getValue<BoxHeightStyle>(
        'selection-height-style',
      ),
      selectionWidthStyle: properties.getValue<BoxWidthStyle>(
        'selection-width-style',
      ),
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
      selectAllOnFocus: properties.getBool('select-all-on-focus') ?? false,
      selectionControls: properties.getValue<TextSelectionControls>(
        'selection-controls',
      ),
      onTap: properties.getVoidCallback('on-tap')?.call(runtime),
      onTapAlwaysCalled: properties.getBool('on-tap-always-called') ?? false,
      onTapOutside: properties
          .getCallback<PointerDownEvent>('on-tap-outside')
          ?.call(runtime),
      onTapUpOutside: properties
          .getCallback<PointerUpEvent>('on-tap-up-outside')
          ?.call(runtime),
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      scrollController: properties.getValue<ScrollController>(
        'scroll-controller',
      ),
      scrollPhysics: properties.getValue<ScrollPhysics>('scroll-physics'),
      autofillHints: properties.getValue<List<String>>('autofill-hints'),
      contentInsertionConfiguration: properties
          .getValue<ContentInsertionConfiguration>(
            'content-insertion-configuration',
          ),
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
      restorationId: properties.getString('restoration-id'),
      stylusHandwritingEnabled:
          properties.getBool('stylus-handwriting-enabled') ?? true,
      enableIMEPersonalizedLearning:
          properties.getBool('enable-ime-personalized-learning') ?? true,
      canRequestFocus: properties.getBool('can-request-focus') ?? true,
      spellCheckConfiguration: properties.getValue<SpellCheckConfiguration>(
        'spell-check-configuration',
      ),
      magnifierConfiguration: properties.getValue<TextMagnifierConfiguration>(
        'magnifier-configuration',
      ),
      hintLocales: properties.getValue<List<Locale>>('hint-locales'),
    );
    return IrNativeValue(Value(textFieldWidget));
  });
}
