import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glue/ir.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/core.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/cross_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/font_weight.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/main_axis_alignment.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/text_align.dart';
import 'package:glue_flutter/src/lib/ui/material/styles/colors.dart';

/// Helper function to extract enum value from Value only (no parsing)
T? extractEnumValue<T>(Ir? ir) {
  if (ir == null) return null;

  // Only accept direct enum objects - no string parsing
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    if (hostValue.value is T) {
      return hostValue.value as T;
    }
  }

  return null; // No fallback parsing
}

/// Extract color value - handles both Color objects and hex string parsing
Color? extractColorValue(Ir? ir) {
  if (ir == null) return null;

  // If it's a direct Color object, use it
  if (ir is IrNativeValue) {
    final hostValue = ir.value;
    if (hostValue.value is Color) {
      return hostValue.value as Color;
    }
  }

  // Otherwise, parse as string (hex colors only)
  return null; // No parsing for unit tests
}

void main() {
  group('UI Module', () {
    test('module is properly defined', () {
      expect(uiCoreModule, isA<ModuleInfo>());
      expect(uiCoreModule.moduleName, 'ffi.ui');
      expect(uiCoreModule.exports, contains('text'));
      expect(uiCoreModule.exports, contains('button'));
      expect(uiCoreModule.exports, contains('container'));
      expect(uiCoreModule.exports, contains('sized-box'));
      expect(uiCoreModule.exports, contains('column'));
      expect(uiCoreModule.exports, contains('row'));
      expect(uiCoreModule.exports, contains('center'));
      expect(uiCoreModule.exports, contains('cross-axis-alignment'));
      expect(uiCoreModule.exports, contains('main-axis-alignment'));
      expect(uiCoreModule.exports, contains('text-align'));
      expect(uiCoreModule.exports, contains('font-weight'));
      expect(uiCoreModule.exports, contains('colors'));
    });

    test('core functions return IrNativeFunc', () {
      final textDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'text',
      );
      expect(textDef.$2, isA<IrNativeFunc>());

      final buttonDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'button',
      );
      expect(buttonDef.$2, isA<IrNativeFunc>());

      final columnDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'column',
      );
      expect(columnDef.$2, isA<IrNativeFunc>());
    });

    test('enum objects are exported', () {
      final crossAxisDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'cross-axis-alignment',
      );
      expect(crossAxisDef.$2, isA<IrObject>());

      final mainAxisDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'main-axis-alignment',
      );
      expect(mainAxisDef.$2, isA<IrObject>());

      final textAlignDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'text-align',
      );
      expect(textAlignDef.$2, isA<IrObject>());

      final fontWeightDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'font-weight',
      );
      expect(fontWeightDef.$2, isA<IrObject>());

      final colorsDef = uiCoreModule.definitions.firstWhere(
        (def) => def.$1 == 'colors',
      );
      expect(colorsDef.$2, isA<IrObject>());
    });
  });

  group('Cross Axis Alignment Enum', () {
    test('has all required values', () {
      expect((crossAxisAlignment).properties['start'], isA<IrNativeValue>());
      expect((crossAxisAlignment).properties['end'], isA<IrNativeValue>());
      expect((crossAxisAlignment).properties['center'], isA<IrNativeValue>());
      expect((crossAxisAlignment).properties['stretch'], isA<IrNativeValue>());
      expect((crossAxisAlignment).properties['baseline'], isA<IrNativeValue>());
    });

    test('values are correct Flutter enums', () {
      final startValue =
          ((crossAxisAlignment).properties['start'] as IrNativeValue).value;
      expect(startValue.value, CrossAxisAlignment.start);

      final centerValue =
          ((crossAxisAlignment).properties['center'] as IrNativeValue).value;
      expect(centerValue.value, CrossAxisAlignment.center);
    });
  });

  group('Main Axis Alignment Enum', () {
    test('has all required values', () {
      expect((mainAxisAlignment).properties['start'], isA<IrNativeValue>());
      expect((mainAxisAlignment).properties['end'], isA<IrNativeValue>());
      expect((mainAxisAlignment).properties['center'], isA<IrNativeValue>());
      expect(
        (mainAxisAlignment).properties['spaceBetween'],
        isA<IrNativeValue>(),
      );
      expect(
        (mainAxisAlignment).properties['spaceAround'],
        isA<IrNativeValue>(),
      );
      expect(
        (mainAxisAlignment).properties['spaceEvenly'],
        isA<IrNativeValue>(),
      );
    });

    test('values are correct Flutter enums', () {
      final startValue =
          ((mainAxisAlignment).properties['start'] as IrNativeValue).value;
      expect(startValue.value, MainAxisAlignment.start);

      final centerValue =
          ((mainAxisAlignment).properties['center'] as IrNativeValue).value;
      expect(centerValue.value, MainAxisAlignment.center);
    });
  });

  group('Text Align Enum', () {
    test('has all required values', () {
      expect((textAlign).properties['left'], isA<IrNativeValue>());
      expect((textAlign).properties['right'], isA<IrNativeValue>());
      expect((textAlign).properties['center'], isA<IrNativeValue>());
      expect((textAlign).properties['justify'], isA<IrNativeValue>());
      expect((textAlign).properties['start'], isA<IrNativeValue>());
      expect((textAlign).properties['end'], isA<IrNativeValue>());
    });

    test('values are correct Flutter enums', () {
      final leftValue = ((textAlign).properties['left'] as IrNativeValue).value;
      expect(leftValue.value, TextAlign.left);

      final centerValue =
          ((textAlign).properties['center'] as IrNativeValue).value;
      expect(centerValue.value, TextAlign.center);
    });
  });

  group('Font Weight Enum', () {
    test('has all required values', () {
      expect((fontWeight).properties['normal'], isA<IrNativeValue>());
      expect((fontWeight).properties['bold'], isA<IrNativeValue>());
      expect((fontWeight).properties['w100'], isA<IrNativeValue>());
      expect((fontWeight).properties['w200'], isA<IrNativeValue>());
      expect((fontWeight).properties['w300'], isA<IrNativeValue>());
      expect((fontWeight).properties['w400'], isA<IrNativeValue>());
      expect((fontWeight).properties['w500'], isA<IrNativeValue>());
      expect((fontWeight).properties['w600'], isA<IrNativeValue>());
      expect((fontWeight).properties['w700'], isA<IrNativeValue>());
      expect((fontWeight).properties['w800'], isA<IrNativeValue>());
      expect((fontWeight).properties['w900'], isA<IrNativeValue>());
    });

    test('values are correct Flutter enums', () {
      final normalValue =
          ((fontWeight).properties['normal'] as IrNativeValue).value;
      expect(normalValue.value, FontWeight.normal);

      final boldValue =
          ((fontWeight).properties['bold'] as IrNativeValue).value;
      expect(boldValue.value, FontWeight.bold);
    });
  });

  group('Colors Enum', () {
    test('has Material Design colors', () {
      expect((colors).properties['red'], isA<IrNativeValue>());
      expect((colors).properties['blue'], isA<IrNativeValue>());
      expect((colors).properties['green'], isA<IrNativeValue>());
      expect((colors).properties['black'], isA<IrNativeValue>());
      expect((colors).properties['white'], isA<IrNativeValue>());
      expect((colors).properties['transparent'], isA<IrNativeValue>());
    });

    test('values are correct Flutter colors', () {
      final redValue = ((colors).properties['red'] as IrNativeValue).value;
      expect(redValue.value, Colors.red);

      final blueValue = ((colors).properties['blue'] as IrNativeValue).value;
      expect(blueValue.value, Colors.blue);

      final blackValue = ((colors).properties['black'] as IrNativeValue).value;
      expect(blackValue.value, Colors.black);
    });
  });

  group('Enum Value Extraction', () {
    test('extractEnumValue extracts FontWeight correctly', () {
      final ir = IrNativeValue(Value(FontWeight.bold));
      expect(extractEnumValue<FontWeight>(ir), FontWeight.bold);
    });

    test('extractEnumValue extracts CrossAxisAlignment correctly', () {
      final ir = IrNativeValue(Value(CrossAxisAlignment.center));
      expect(
        extractEnumValue<CrossAxisAlignment>(ir),
        CrossAxisAlignment.center,
      );
    });

    test('extractEnumValue returns null for wrong type', () {
      final ir = IrNativeValue(Value(FontWeight.bold));
      expect(extractEnumValue<CrossAxisAlignment>(ir), null);
    });

    test('extractEnumValue returns null for non-Value', () {
      final ir = IrString('bold');
      expect(extractEnumValue<FontWeight>(ir), null);
    });
  });

  group('Color Value Extraction', () {
    test('extractColorValue extracts Color correctly', () {
      final ir = IrNativeValue(Value(Colors.blue));
      expect(extractColorValue(ir), Colors.blue);
    });

    test('extractColorValue returns null for non-Color', () {
      final ir = IrNativeValue(Value(FontWeight.bold));
      expect(extractColorValue(ir), null);
    });

    test('extractColorValue returns null for non-Value', () {
      final ir = IrString('#FF0000');
      expect(extractColorValue(ir), null);
    });
  });
}
