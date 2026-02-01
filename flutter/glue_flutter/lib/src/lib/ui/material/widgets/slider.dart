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
  return getRuntime().map((runtime) {
    final sliderWidget = Slider(
      key: properties.key,
      value: properties.getDouble('value') ?? 0.0,
      secondaryTrackValue: properties.getDouble('secondary-track-value'),
      onChanged: properties.getCallback<double>('on-changed')?.call(runtime),
      onChangeStart: properties
          .getCallback<double>('on-change-start')
          ?.call(runtime),
      onChangeEnd: properties
          .getCallback<double>('on-change-end')
          ?.call(runtime),
      min: properties.getDouble('min') ?? 0.0,
      max: properties.getDouble('max') ?? 1.0,
      divisions: properties.getInt('divisions'),
      label: properties.getString('label'),
      activeColor: properties.getColor('active-color'),
      inactiveColor: properties.getColor('inactive-color'),
      secondaryActiveColor: properties.getColor('secondary-active-color'),
      thumbColor: properties.getColor('thumb-color'),
      overlayColor: properties.getValue<WidgetStateProperty<Color?>>(
        'overlay-color',
      ),
      mouseCursor: properties.getValue<MouseCursor>('mouse-cursor'),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      allowedInteraction: properties.getValue<SliderInteraction>(
        'allowed-interaction',
      ),
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    );
    return IrNativeValue(Value(sliderWidget));
  });
}
