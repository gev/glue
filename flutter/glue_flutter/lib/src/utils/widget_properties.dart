import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Properties wrapper with lazy getters
/// Provides clean API for accessing widget properties without pre-computing everything
class WidgetProperties {
  final Map<String, Ir> _props;

  WidgetProperties(Map<String, Ir> props) : _props = props;
  WidgetProperties.empty() : _props = {};

  String? getString(String key) => extractString(_props[key]);
  bool? getBool(String key) => extractBool(_props[key]);
  double? getDouble(String key) => extractDouble(_props[key]);
  int? getInt(String key) => extractInt(_props[key]);

  T? getValue<T>(String key) => extractNativeValue(_props[key]);
  List<T> getValues<T>(String key) => extractNativeValues(_props[key]);

  Key? getKey(String key) => extractNativeValue(_props[key]);

  Widget? getWidget(String key) => extractNativeValue(_props[key]);
  List<Widget> getWidgets(String key) => extractNativeValues(_props[key]);

  Color? getColor(String key) => extractColor(_props[key]);

  VoidCallback? getVoidCallback(String key, Runtime runtime) =>
      extractVoidCallback(_props[key], runtime);

  double? get width => extractDouble(_props['width']);
  double? get height => extractDouble(_props['height']);
  double? get top => extractDouble(_props['top']);
  double? get bottom => extractDouble(_props['bottom']);
  double? get left => extractDouble(_props['left']);
  double? get right => extractDouble(_props['right']);
  double? get start => extractDouble(_props['start']);
  double? get end => extractDouble(_props['end']);
  double? get horizontal => extractDouble(_props['horizontal']);
  double? get vertical => extractDouble(_props['vertical']);

  Key? get key => extractNativeValue(_props['key']);

  Widget? get child => extractNativeValue(_props['child']);
  List<Widget> get children => extractNativeValues(_props['children']);
}
