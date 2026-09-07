import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SegmentedButtonStyle binder for Glue using official styleFrom
final Ir segmentedButtonStyle = IrNativeFunc(segmentedButtonStyleImpl);

Eval<Ir> segmentedButtonStyleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSegmentedButtonStyle(
    WidgetProperties(properties.unlock),
  ),
  _ => Eval.pure(IrNativeValue(Value(const ButtonStyle()))),
};

Eval<Ir> _createSegmentedButtonStyle(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final style = SegmentedButton.styleFrom(
      foregroundColor: properties.getValue<Color>('foreground-color'),
      backgroundColor: properties.getValue<Color>('background-color'),
      selectedForegroundColor: properties.getValue<Color>(
        'selected-foreground-color',
      ),
      selectedBackgroundColor: properties.getValue<Color>(
        'selected-background-color',
      ),
      disabledForegroundColor: properties.getValue<Color>(
        'disabled-foreground-color',
      ),
      disabledBackgroundColor: properties.getValue<Color>(
        'disabled-background-color',
      ),
      shadowColor: properties.getValue<Color>('shadow-color'),
      surfaceTintColor: properties.getValue<Color>('surface-tint-color'),
      iconColor: properties.getValue<Color>('icon-color'),
      iconSize: properties.getDouble('icon-size'),
      disabledIconColor: properties.getValue<Color>('disabled-icon-color'),
      overlayColor: properties.getValue<Color>('overlay-color'),
      elevation: properties.getDouble('elevation'),
      textStyle: properties.getValue<TextStyle>('text-style'),
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
      minimumSize: properties.getValue<Size>('minimum-size'),
      fixedSize: properties.getValue<Size>('fixed-size'),
      maximumSize: properties.getValue<Size>('maximum-size'),
      side: properties.getValue<BorderSide>('side'),
      shape: properties.getValue<OutlinedBorder>('shape'),
      enabledMouseCursor: properties.getValue<MouseCursor>(
        'enabled-mouse-cursor',
      ),
      disabledMouseCursor: properties.getValue<MouseCursor>(
        'disabled-mouse-cursor',
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
    );

    return IrNativeValue(Value(style));
  });
}
