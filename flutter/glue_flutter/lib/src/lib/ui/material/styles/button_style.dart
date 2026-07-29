import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ButtonStyle binder for Glue
final Ir buttonStyle = IrNativeFunc(buttonStyleImpl);

Eval<Ir> buttonStyleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createButtonStyle(
    WidgetProperties(properties.unlock),
  ),
  _ => Eval.pure(IrNativeValue(Value(const ButtonStyle()))),
};

Eval<Ir> _createButtonStyle(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final style = ButtonStyle(
      textStyle: _wsp(properties.getValue<TextStyle>('text-style')),
      backgroundColor: _wsp(properties.getValue<Color>('background-color')),
      foregroundColor: _wsp(properties.getValue<Color>('foreground-color')),
      overlayColor: _wsp(properties.getValue<Color>('overlay-color')),
      shadowColor: _wsp(properties.getValue<Color>('shadow-color')),
      surfaceTintColor: _wsp(properties.getValue<Color>('surface-tint-color')),
      elevation: _wsp(properties.getDouble('elevation')),
      padding: _wsp(properties.getValue<EdgeInsetsGeometry>('padding')),
      minimumSize: _wsp(properties.getValue<Size>('minimum-size')),
      fixedSize: _wsp(properties.getValue<Size>('fixed-size')),
      maximumSize: _wsp(properties.getValue<Size>('maximum-size')),
      iconColor: _wsp(properties.getValue<Color>('icon-color')),
      iconSize: _wsp(properties.getDouble('icon-size')),
      iconAlignment: properties.getValue<IconAlignment>('icon-alignment'),
      side: _wsp(properties.getValue<BorderSide>('side')),
      shape: _wsp(properties.getValue<OutlinedBorder>('shape')),
      mouseCursor: _wsp(properties.getValue<MouseCursor>('mouse-cursor')),
      visualDensity: properties.getValue<VisualDensity>('visual-density'),
      tapTargetSize: properties.getValue<MaterialTapTargetSize>(
        'tap-target-size',
      ),
      animationDuration: properties.getValue<Duration>('animation-duration'),
      enableFeedback: properties.getBool('enable-feedback'),
      alignment: properties.getValue<AlignmentGeometry>('alignment'),
      splashFactory: properties.getValue<InteractiveInkFeatureFactory>(
        'splash-factory',
      ),
      backgroundBuilder: properties.getValue<ButtonLayerBuilder>(
        'background-builder',
      ),
      foregroundBuilder: properties.getValue<ButtonLayerBuilder>(
        'foreground-builder',
      ),
    );

    return IrNativeValue(Value(style));
  });
}

WidgetStateProperty<T>? _wsp<T>(T? value) =>
    value != null ? WidgetStateProperty.all(value) : null;
