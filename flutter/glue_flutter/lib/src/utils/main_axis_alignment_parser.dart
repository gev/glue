import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse main axis alignment from Glue IR value
MainAxisAlignment? parseMainAxisAlignment(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => _parseMainAxisAlignmentString(alignStr),
    _ => null,
  };
}

/// Parse main axis alignment from string
MainAxisAlignment? _parseMainAxisAlignmentString(String align) {
  return switch (align.toLowerCase()) {
    'start' => MainAxisAlignment.start,
    'end' => MainAxisAlignment.end,
    'center' => MainAxisAlignment.center,
    'spacebetween' => MainAxisAlignment.spaceBetween,
    'spacearound' => MainAxisAlignment.spaceAround,
    'spaceevenly' => MainAxisAlignment.spaceEvenly,
    _ => null,
  };
}
