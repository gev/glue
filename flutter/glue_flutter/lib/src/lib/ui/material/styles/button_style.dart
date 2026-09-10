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
      textStyle: wrapWidgetStateProperty(
        properties.getValue<TextStyle>('text-style'),
      ),
      backgroundColor: wrapWidgetStateProperty(
        properties.getValue<Color>('background-color'),
      ),
      foregroundColor: wrapWidgetStateProperty(
        properties.getValue<Color>('foreground-color'),
      ),
      overlayColor: wrapWidgetStateProperty(
        properties.getValue<Color>('overlay-color'),
      ),
      shadowColor: wrapWidgetStateProperty(
        properties.getValue<Color>('shadow-color'),
      ),
      surfaceTintColor: wrapWidgetStateProperty(
        properties.getValue<Color>('surface-tint-color'),
      ),
      elevation: wrapWidgetStateProperty(properties.getDouble('elevation')),
      padding: wrapWidgetStateProperty(
        properties.getValue<EdgeInsetsGeometry>('padding'),
      ),
      minimumSize: wrapWidgetStateProperty(
        properties.getValue<Size>('minimum-size'),
      ),
      fixedSize: wrapWidgetStateProperty(
        properties.getValue<Size>('fixed-size'),
      ),
      maximumSize: wrapWidgetStateProperty(
        properties.getValue<Size>('maximum-size'),
      ),
      iconColor: wrapWidgetStateProperty(
        properties.getValue<Color>('icon-color'),
      ),
      iconSize: wrapWidgetStateProperty(properties.getDouble('icon-size')),
      iconAlignment: properties.getValue<IconAlignment>('icon-alignment'),
      side: wrapWidgetStateProperty(properties.getValue<BorderSide>('side')),
      shape: wrapWidgetStateProperty(
        properties.getValue<OutlinedBorder>('shape'),
      ),
      mouseCursor: wrapWidgetStateProperty(
        properties.getValue<MouseCursor>('mouse-cursor'),
      ),
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
