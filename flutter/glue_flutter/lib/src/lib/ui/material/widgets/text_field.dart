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
      groupId: properties.getValue<>('group-id'),
      controller: properties.getValue<>('controller'),
      focusNode: properties.getValue<>('focus-node'),
      undoController: properties.getValue<>('undo-controller'),
      decoration: properties.getValue<>('decoration') ?? const InputDecoration(),
      keyboardType: properties.getValue<>('keyboard-type'),
      textInputAction: properties.getValue<>('text-input-action'),
      textCapitalization: properties.getValue<>('text-capitalization'),
      style: properties.getValue<>('style'),
      strutStyle: properties.getValue<>('strut-style'),
      textAlign: properties.getValue<>('text-align') ?? TextAlign.start,
      textAlignVertical: properties.getValue<>('text-align-vertical'),
      textDirection: properties.getValue<>('text-direction'),
      readOnly: properties.getBool('read-only') ?? false,
      toolbarOptions: properties.getValue<>('toolbar-options'),
      showCursor: properties.getBool('show-cursor'),
      autofocus: properties.getBool('autofocus') ?? false,
      statesController: properties.getValue<>('states-controller'),
      obscuringCharacter: properties.getString('obscuring-character') ?? '•',
      obscureText: properties.getBool('obscure-text') ?? false,
      autocorrect: properties.getBool('autocorrect') ?? true,
      smartDashesType:
          properties.getValue<>('smart-dashes-type') ?? SmartDashesType.enabled,
      smartQuotesType:
          properties.getValue<>('smart-quotes-type') ?? SmartQuotesType.enabled,
      enableSuggestions: properties.getBool('enable-suggestions') ?? true,
      maxLines: properties.getInt('max-lines') ?? 1,
      minLines: properties.getInt('min-lines'),
      expands: properties.getBool('expands') ?? false,
      maxLength: properties.getInt('max-length'),
      maxLengthEnforcement: properties.getValue<>('max-length-enforcement'),
      onChanged: properties.getValue<>('on-changed'),
      onEditingComplete: properties.getVoidCallback(
        'on-editing-complete',
        runtime,
      ),
      onSubmitted: properties.getValue<>('on-submitted'),
      onAppPrivateCommand: properties.getValue<>('on-app-private-command'),
      inputFormatters: properties.getValue<>('input-formatters'),
      enabled: properties.getBool('enabled') ?? true,
      ignorePointers: properties.getBool('ignore-pointers'),
      cursorWidth: properties.getDouble('cursor-width') ?? 2.0,
      cursorHeight: properties.getDouble('cursor-height') ?? 16.0,
      cursorRadius: properties.getValue<>('cursor-radius'),
      cursorOpacityAnimates: properties.getBool('cursor-opacity-animates'),
      cursorColor: properties.getColor('cursor-color'),
      cursorErrorColor: properties.getColor('cursor-error-color'),
      selectionHeightStyle: properties.getValue<>('selection-height-style'),
      selectionWidthStyle: properties.getValue<>('selection-width-style'),
      keyboardAppearance: properties.getValue<>('keyboard-appearance'),
      scrollPadding: properties.getValue<>('scroll-padding'),
      dragStartBehavior: properties.getValue<>('drag-start-behavior'),
      enableInteractiveSelection:
          properties.getBool('enable-interactive-selection') ?? true,
      selectAllOnFocus: properties.getBool('select-all-on-focus') ?? false,
      selectionControls: properties.getValue<>('selection-controls'),
      onTap: properties.getVoidCallback('on-tap', runtime),
      onTapAlwaysCalled: properties.getBool('on-tap-always-called') ?? false,
      onTapOutside: properties.getValue<>('on-tap-outside'),
      onTapUpOutside: properties.getValue<>('on-tap-up-outside'),
      mouseCursor: properties.getValue<>('mouse-cursor'),
      buildCounter: properties.getValue<>('build-counter'),
      scrollController: properties.getValue<>('scroll-controller'),
      scrollPhysics: properties.getValue<>('scroll-physics'),
      autofillHints: properties.getValue<>('autofill-hints'),
      contentInsertionConfiguration: properties.getValue<>(
        'content-insertion-configuration',
      ),
      clipBehavior: properties.getValue<>('clip-behavior'),
      restorationId: properties.getString('restoration-id'),
      scribbleEnabled: properties.getBool('scribble-enabled') ?? true,
      stylusHandwritingEnabled:
          properties.getBool('stylus-handwriting-enabled') ?? true,
      enableIMEPersonalizedLearning:
          properties.getBool('enable-ime-personalized-learning') ?? true,
      contextMenuBuilder: properties.getValue<>('context-menu-builder'),
      canRequestFocus: properties.getBool('can-request-focus') ?? true,
      spellCheckConfiguration: properties.getValue<>('spell-check-configuration'),
      magnifierConfiguration: properties.getValue<>('magnifier-configuration'),
      hintLocales: properties.getValue<>('hint-locales'),
    );
    return IrNativeValue(Value(textFieldWidget));
  });
}
