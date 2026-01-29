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
      controller: properties.textEditingController,
      focusNode: properties.focusNode,
      decoration: properties.inputDecoration ?? const InputDecoration(),
      keyboardType: properties.keyboardType,
      textInputAction: properties.textInputAction,
      textCapitalization: properties.textCapitalization,
      style: properties.textStyle,
      textAlign: properties.align ?? TextAlign.start,
      textAlignVertical: properties.textAlignVertical,
      textDirection: properties.textDirection,
      readOnly: properties.readOnly,
      autofocus: properties.textFieldAutofocus,
      obscuringCharacter: properties.obscuringCharacter,
      obscureText: properties.obscureText,
      autocorrect: true, // default to true
      smartDashesType: SmartDashesType.enabled,
      smartQuotesType: SmartQuotesType.enabled,
      enableSuggestions: properties.enableSuggestions,
      maxLines: properties.textFieldMaxLines ?? 1,
      minLines: properties.minLines,
      expands: properties.expands,
      maxLength: properties.maxLength,
      maxLengthEnforcement: properties.maxLengthEnforcement,
      onChanged: properties.onTextChanged,
      onEditingComplete: properties.onEditingComplete(runtime),
      onSubmitted: properties.onSubmitted,
      inputFormatters: properties.inputFormatters,
      enabled: properties.textFieldEnabled,
      cursorWidth: properties.cursorWidth,
      cursorHeight: properties.cursorHeight,
      cursorRadius: properties.cursorRadius,
      cursorColor: properties.cursorColor,
      cursorErrorColor: properties.cursorErrorColor,
      keyboardAppearance: properties.keyboardAppearance,
      scrollPadding: properties.textFieldScrollPadding,
      enableInteractiveSelection: properties.enableInteractiveSelection,
      selectAllOnFocus: properties.selectAllOnFocus,
      selectionControls: properties.selectionControls,
      onTap: properties.onTextFieldTap,
      onTapAlwaysCalled: properties.onTapAlwaysCalled,
      mouseCursor: properties.textFieldMouseCursor,
      scrollController: properties.textFieldScrollController,
      scrollPhysics: properties.scrollPhysics,
      autofillHints: properties.autofillHints,
      clipBehavior: properties.clipBehavior,
      restorationId: properties.textFieldRestorationId,
    );
    return IrNativeValue(Value(textFieldWidget));
  });
}
