import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse cross axis alignment from Glue IR value
CrossAxisAlignment? parseCrossAxisAlignment(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => _parseCrossAxisAlignmentString(alignStr),
    _ => null,
  };
}

/// Parse cross axis alignment from string
CrossAxisAlignment? _parseCrossAxisAlignmentString(String align) {
  return switch (align.toLowerCase()) {
    'start' => CrossAxisAlignment.start,
    'end' => CrossAxisAlignment.end,
    'center' => CrossAxisAlignment.center,
    'stretch' => CrossAxisAlignment.stretch,
    'baseline' => CrossAxisAlignment.baseline,
    _ => null,
  };
}
