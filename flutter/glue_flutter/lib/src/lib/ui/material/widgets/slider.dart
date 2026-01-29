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
    value: properties.sliderValue,
    secondaryTrackValue: properties.sliderSecondaryTrackValue,
    onChanged: properties.onSliderChanged,
    onChangeStart: properties.onSliderChangeStart,
    onChangeEnd: properties.onSliderChangeEnd,
    min: properties.sliderMin,
    max: properties.sliderMax,
    divisions: properties.sliderDivisions,
    label: properties.sliderLabel,
    activeColor: properties.activeColor,
    inactiveColor: properties.color, // using color for inactive
    secondaryActiveColor:
        properties.selectedColor, // using selectedColor for secondary
    thumbColor: properties.focusColor, // using focusColor for thumb
    overlayColor: properties.overlayColor,
    mouseCursor: properties.mouseCursor,
    semanticFormatterCallback: properties.semanticFormatterCallback,
    focusNode: properties.focusNode,
    autofocus: properties.autofocus,
    allowedInteraction: properties.allowedInteraction,
  );
  return Eval.pure(IrNativeValue(Value(sliderWidget)));
}
