import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Slider widget function
/// Creates Flutter Slider from Glue (slider props) expressions
final Ir slider = IrNativeFunc(sliderImpl);

/// Slider implementation - takes properties object
Eval<Ir> sliderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSlider(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSlider(WidgetProperties.empty()),
};

/// Create Slider widget from properties
Eval<Ir> _createSlider(WidgetProperties properties) {
  final sliderWidget = Slider(
    key: properties.key,
    value: properties.getDouble('value') ?? 0.0,
    secondaryTrackValue: properties.getDouble('secondary-track-value'),
    onChanged: properties.getValue('on-changed'),
    onChangeStart: properties.getValue('on-change-start'),
    onChangeEnd: properties.getValue('on-change-end'),
    min: properties.getDouble('min') ?? 0.0,
    max: properties.getDouble('max') ?? 1.0,
    divisions: properties.getInt('divisions'),
    label: properties.getString('label'),
    activeColor: properties.getColor('active-color'),
    inactiveColor: properties.getColor('inactive-color'),
    secondaryActiveColor: properties.getColor('secondary-active-color'),
    thumbColor: properties.getColor('thumb-color'),
    overlayColor: properties.getValue('overlay-color'),
    mouseCursor: properties.getValue('mouse-cursor'),
    semanticFormatterCallback: properties.getValue(
      'semantic-formatter-callback',
    ),
    focusNode: properties.getValue('focus-node'),
    autofocus: properties.getBool('autofocus') ?? false,
    allowedInteraction: properties.getValue('allowed-interaction'),
  );
  return Eval.pure(IrNativeValue(Value(sliderWidget)));
}
