import 'package:flutter/material.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class Properties {
  final Map<String, dynamic> _props;

  const Properties(this._props);

  // Button properties
  String get label => extractString(_props['label']) ?? 'Button';
  VoidCallback? get onTap => extractVoidCallback(_props['on-tap']);
  bool get disabled => extractBool(_props['disabled']) ?? false;

  // Text properties
  Color? get color => extractColor(_props['color']);
  double? get size => extractDouble(_props['size']);
  FontWeight? get weight => extractFontWeight(_props['weight']);
  TextAlign? get align => extractTextAlign(_props['align']);

  // Layout properties
  List<Widget> get children => extractChildren(_props['children']) ?? [];
  MainAxisAlignment get mainAlign =>
      extractMainAxisAlignment(_props['main-axis-align']) ??
      MainAxisAlignment.start;
  CrossAxisAlignment get crossAlign =>
      extractCrossAxisAlignment(_props['cross-axis-align']) ??
      CrossAxisAlignment.start;
  Axis get direction => extractAxis(_props['direction']) ?? Axis.vertical;
  double? get spacing => extractDouble(_props['spacing']);

  // Container properties
  EdgeInsetsGeometry get padding =>
      extractEdgeInsets(_props['padding']) ?? EdgeInsets.zero;
}
