import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse text alignment from Glue IR value
TextAlign? parseTextAlign(Ir ir) {
  return switch (ir) {
    IrString(value: final alignStr) => _parseTextAlignString(alignStr),
    _ => null,
  };
}

/// Parse text alignment from string
TextAlign? _parseTextAlignString(String align) {
  return switch (align.toLowerCase()) {
    'left' => TextAlign.left,
    'right' => TextAlign.right,
    'center' => TextAlign.center,
    'justify' => TextAlign.justify,
    'start' => TextAlign.start,
    'end' => TextAlign.end,
    _ => null,
  };
}
