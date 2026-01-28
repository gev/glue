import 'package:flutter/material.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class Properties {
  final Map<String, Ir> _props;

  Properties(Map<String, Ir> props) : _props = props {
    print('Created Properties: ${props}');
  }

  /// Creates empty properties with all defaults
  Properties.empty() : _props = {};

  // Button properties
  String? get label => extractString(_props['label']);
  VoidCallback? onTap(Runtime runtime) =>
      extractVoidCallback(_props['on-tap'], runtime);
  VoidCallback? onLongPress(Runtime runtime) =>
      extractVoidCallback(_props['on-long-press'], runtime);
  ValueChanged<bool>? get onHover =>
      extractNativeValue<ValueChanged<bool>>(_props['on-hover']);
  ValueChanged<bool>? get onFocusChange =>
      extractNativeValue<ValueChanged<bool>>(_props['on-focus-change']);
  ButtonStyle? get buttonStyle =>
      extractNativeValue<ButtonStyle>(_props['style']);
  FocusNode? get focusNode =>
      extractNativeValue<FocusNode>(_props['focus-node']);
  bool get autofocus => extractBool(_props['autofocus']) ?? false;
  Clip get buttonClipBehavior =>
      extractClip(_props['clip-behavior']) ?? Clip.none;
  bool get disabled => extractBool(_props['disabled']) ?? false;

  // Text properties
  String? get content => extractString(_props['content']);
  Color? get color => extractColor(_props['color']);
  double? get size => extractDouble(_props['size']);
  FontWeight? get weight => extractFontWeight(_props['weight']);
  TextAlign? get align => extractTextAlign(_props['align']);
  TextOverflow? get overflow => extractTextOverflow(_props['overflow']);
  double? get textScaleFactor => extractDouble(_props['text-scale-factor']);
  int? get maxLines => extractInt(_props['max-lines']);
  String? get semanticsLabel => extractString(_props['semantics-label']);
  TextWidthBasis? get textWidthBasis =>
      extractTextWidthBasis(_props['text-width-basis']);
  TextHeightBehavior? get textHeightBehavior =>
      extractNativeValue<TextHeightBehavior>(_props['text-height-behavior']);
  bool? get softWrap => extractBool(_props['soft-wrap']);
  Locale? get locale => extractNativeValue<Locale>(_props['locale']);
  StrutStyle? get strutStyle =>
      extractNativeValue<StrutStyle>(_props['strut-style']);

  // Layout properties
  List<Widget> get children => extractChildren(_props['children']) ?? [];
  Widget? get child => extractChild(_props['child']);
  MainAxisAlignment get mainAlign =>
      extractMainAxisAlignment(_props['main-axis-align']) ??
      MainAxisAlignment.start;
  CrossAxisAlignment get crossAlign =>
      extractCrossAxisAlignment(_props['cross-axis-align']) ??
      CrossAxisAlignment.start;
  Axis get direction => extractAxis(_props['direction']) ?? Axis.vertical;
  double? get spacing => extractDouble(_props['spacing']);
  MainAxisSize get mainAxisSize =>
      extractMainAxisSize(_props['main-axis-size']) ?? MainAxisSize.max;
  TextDirection? get textDirection =>
      extractTextDirection(_props['text-direction']);
  VerticalDirection get verticalDirection =>
      extractVerticalDirection(_props['vertical-direction']) ??
      VerticalDirection.down;
  TextBaseline? get textBaseline =>
      extractTextBaseline(_props['text-baseline']);

  // Container properties
  EdgeInsetsGeometry get padding =>
      extractEdgeInsets(_props['padding']) ?? EdgeInsets.zero;
  AlignmentGeometry? get alignment =>
      extractNativeValue<AlignmentGeometry>(_props['alignment']);
  double? get width => extractDouble(_props['width']);
  double? get height => extractDouble(_props['height']);
  BoxConstraints? get constraints =>
      extractNativeValue<BoxConstraints>(_props['constraints']);
  EdgeInsetsGeometry? get margin => extractEdgeInsets(_props['margin']);
  Decoration? get decoration =>
      extractNativeValue<Decoration>(_props['decoration']);
  Decoration? get foregroundDecoration =>
      extractNativeValue<Decoration>(_props['foreground-decoration']);
  Matrix4? get transform => extractNativeValue<Matrix4>(_props['transform']);
  AlignmentGeometry? get transformAlignment =>
      extractNativeValue<AlignmentGeometry>(_props['transform-alignment']);
  Clip get clipBehavior => extractClip(_props['clip-behavior']) ?? Clip.none;
}
