import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Parse edge insets from Glue IR value
EdgeInsets? parseEdgeInsets(Ir ir) {
  return switch (ir) {
    IrObject(:final properties) => _parseEdgeInsetsFromObject(properties),
    _ => null,
  };
}

/// Parse edge insets from list
EdgeInsets? _parseEdgeInsetsFromList(dynamic elements) {
  final list = elements.unlock as List<Ir>;
  return switch (list.length) {
    1 => EdgeInsets.all((list[0] as IrFloat).value),
    2 => EdgeInsets.symmetric(
      vertical: (list[0] as IrFloat).value,
      horizontal: (list[1] as IrFloat).value,
    ),
    4 => EdgeInsets.only(
      top: (list[0] as IrFloat).value,
      right: (list[1] as IrFloat).value,
      bottom: (list[2] as IrFloat).value,
      left: (list[3] as IrFloat).value,
    ),
    _ => null,
  };
}

/// Parse edge insets from object properties
EdgeInsets? _parseEdgeInsetsFromObject(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;

  // Single value properties
  if (props.containsKey('all')) {
    final value = extractDouble(props['all']);
    if (value != null) return EdgeInsets.all(value);
  }

  // Symmetric properties
  final vertical = extractDouble(props['vertical']);
  final horizontal = extractDouble(props['horizontal']);
  if (vertical != null && horizontal != null) {
    return EdgeInsets.symmetric(vertical: vertical, horizontal: horizontal);
  }

  // Individual side properties (only start/end, no left/right)
  final top = extractDouble(props['top']) ?? 0;
  final end = extractDouble(props['end']) ?? 0;
  final bottom = extractDouble(props['bottom']) ?? 0;
  final start = extractDouble(props['start']) ?? 0;

  if (top != 0 || end != 0 || bottom != 0 || start != 0) {
    return EdgeInsets.only(top: top, right: end, bottom: bottom, left: start);
  }

  return null;
}
