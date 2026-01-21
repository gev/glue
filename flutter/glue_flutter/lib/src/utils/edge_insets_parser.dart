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

/// Parse edge insets from object properties
EdgeInsets? _parseEdgeInsetsFromObject(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;

  // Single value properties
  if (props.containsKey('all')) {
    final value = extractDouble(props['all']);
    if (value != null) return EdgeInsets.all(value);
  }

  // Symmetric properties (allow either vertical or horizontal)
  final vertical = extractDouble(props['vertical']);
  final horizontal = extractDouble(props['horizontal']);
  if (vertical != null || horizontal != null) {
    return EdgeInsets.symmetric(
      vertical: vertical ?? 0,
      horizontal: horizontal ?? 0,
    );
  }

  // Individual side properties (only start/end, no left/right)
  final hasTop = props.containsKey('top');
  final hasBottom = props.containsKey('bottom');
  final hasStart = props.containsKey('start');
  final hasEnd = props.containsKey('end');

  if (hasTop || hasBottom || hasStart || hasEnd) {
    final top = extractDouble(props['top']) ?? 0;
    final end = extractDouble(props['end']) ?? 0;
    final bottom = extractDouble(props['bottom']) ?? 0;
    final start = extractDouble(props['start']) ?? 0;

    return EdgeInsets.only(top: top, right: end, bottom: bottom, left: start);
  }

  return null;
}
