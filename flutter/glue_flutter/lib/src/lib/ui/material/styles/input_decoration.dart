import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// InputDecoration function
/// Creates Flutter InputDecoration from Glue (input-decoration props) expressions
final Ir inputDecoration = IrNativeFunc(inputDecorationImpl);

/// InputDecoration implementation - takes properties object
Eval<Ir> inputDecorationImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createInputDecoration(
    WidgetProperties(properties.unlock),
  ),
  _ => _createInputDecoration(WidgetProperties.empty()),
};

/// Create InputDecoration from properties
Eval<Ir> _createInputDecoration(WidgetProperties properties) {
  final decoration = InputDecoration(
    icon: properties.getWidget('icon'),
    iconColor: properties.getColor('icon-color'),
    label: properties.getWidget('label'),
    labelText: properties.getString('label-text'),
    labelStyle: properties.getValue<TextStyle>('label-style'),
    floatingLabelStyle: properties.getValue<TextStyle>('floating-label-style'),
    helperText: properties.getString('helper-text'),
    helperStyle: properties.getValue<TextStyle>('helper-style'),
    helperMaxLines: properties.getInt('helper-max-lines'),
    hintText: properties.getString('hint-text'),
    hintStyle: properties.getValue<TextStyle>('hint-style'),
    hintTextDirection: properties.getValue<TextDirection>(
      'hint-text-direction',
    ),
    hintMaxLines: properties.getInt('hint-max-lines'),
    errorText: properties.getString('error-text'),
    errorStyle: properties.getValue<TextStyle>('error-style'),
    errorMaxLines: properties.getInt('error-max-lines'),
    floatingLabelBehavior: properties.getValue<FloatingLabelBehavior>(
      'floating-label-behavior',
    ),
    floatingLabelAlignment: properties.getValue<FloatingLabelAlignment>(
      'floating-label-alignment',
    ),
    isDense: properties.getBool('is-dense'),
    contentPadding: properties.getValue<EdgeInsets>('content-padding'),
    prefixIcon: properties.getWidget('prefix-icon'),
    prefixIconConstraints: properties.getValue<BoxConstraints>(
      'prefix-icon-constraints',
    ),
    prefix: properties.getWidget('prefix'),
    prefixText: properties.getString('prefix-text'),
    prefixStyle: properties.getValue<TextStyle>('prefix-style'),
    prefixIconColor: properties.getColor('prefix-icon-color'),
    suffixIcon: properties.getWidget('suffix-icon'),
    suffix: properties.getWidget('suffix'),
    suffixText: properties.getString('suffix-text'),
    suffixStyle: properties.getValue<TextStyle>('suffix-style'),
    suffixIconColor: properties.getColor('suffix-icon-color'),
    suffixIconConstraints: properties.getValue<BoxConstraints>(
      'suffix-icon-constraints',
    ),
    counter: properties.getWidget('counter'),
    counterText: properties.getString('counter-text'),
    counterStyle: properties.getValue<TextStyle>('counter-style'),
    filled: properties.getBool('filled'),
    fillColor: properties.getColor('fill-color'),
    focusColor: properties.getColor('focus-color'),
    hoverColor: properties.getColor('hover-color'),
    errorBorder: properties.getValue<InputBorder>('error-border'),
    focusedBorder: properties.getValue<InputBorder>('focused-border'),
    focusedErrorBorder: properties.getValue<InputBorder>(
      'focused-error-border',
    ),
    disabledBorder: properties.getValue<InputBorder>('disabled-border'),
    enabledBorder: properties.getValue<InputBorder>('enabled-border'),
    border: properties.getValue<InputBorder>('border'),
    enabled: properties.getBool('enabled') ?? true,
    semanticCounterText: properties.getString('semantic-counter-text'),
    alignLabelWithHint: properties.getBool('align-label-with-hint'),
    constraints: properties.getValue<BoxConstraints>('constraints'),
  );
  return Eval.pure(IrNativeValue(Value(decoration)));
}
