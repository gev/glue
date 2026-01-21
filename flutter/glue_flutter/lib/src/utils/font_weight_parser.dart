import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse font weight from Glue IR value
FontWeight? parseFontWeight(Ir ir) {
  return switch (ir) {
    IrString(value: final weightStr) => _parseFontWeightString(weightStr),
    IrInteger(value: final weightInt) => _parseFontWeightInt(weightInt),
    _ => null,
  };
}

/// Parse font weight from string
FontWeight? _parseFontWeightString(String weight) {
  return switch (weight.toLowerCase()) {
    'normal' => FontWeight.normal,
    'bold' => FontWeight.bold,
    'w100' => FontWeight.w100,
    'w200' => FontWeight.w200,
    'w300' => FontWeight.w300,
    'w400' => FontWeight.w400,
    'w500' => FontWeight.w500,
    'w600' => FontWeight.w600,
    'w700' => FontWeight.w700,
    'w800' => FontWeight.w800,
    'w900' => FontWeight.w900,
    _ => null,
  };
}

/// Parse font weight from integer
FontWeight? _parseFontWeightInt(int weight) {
  return switch (weight) {
    100 => FontWeight.w100,
    200 => FontWeight.w200,
    300 => FontWeight.w300,
    400 => FontWeight.w400,
    500 => FontWeight.w500,
    600 => FontWeight.w600,
    700 => FontWeight.w700,
    800 => FontWeight.w800,
    900 => FontWeight.w900,
    _ => null,
  };
}
