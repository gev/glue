import 'package:glue/src/module.dart';
import 'package:glue/src/lib/math.dart';
import 'package:test/test.dart';

void main() {
  group('Glue.Lib.Math (Main Math Module)', () {
    test('math module loads successfully', () {
      expect(math, isNotNull);
      expect(math.moduleName, equals('ffi.math'));
    });

    test('math module contains all expected functions', () {
      final functionNames = math.definitions.map((pair) {
        final (name, _) = pair;
        return name;
      }).toSet();

      // Arithmetic functions
      expect(functionNames.contains('+'), isTrue);
      expect(functionNames.contains('-'), isTrue);
      expect(functionNames.contains('*'), isTrue);
      expect(functionNames.contains('/'), isTrue);
      expect(functionNames.contains('%'), isTrue);

      // Constants
      expect(functionNames.contains('pi'), isTrue);
      expect(functionNames.contains('e'), isTrue);

      // Logarithmic functions
      expect(functionNames.contains('lg'), isTrue);
      expect(functionNames.contains('ln'), isTrue);
      expect(functionNames.contains('log'), isTrue);

      // Power functions
      expect(functionNames.contains('exp'), isTrue);
      expect(functionNames.contains('pow'), isTrue);
      expect(functionNames.contains('sqrt'), isTrue);

      // Trigonometric functions
      expect(functionNames.contains('sin'), isTrue);
      expect(functionNames.contains('cos'), isTrue);
      expect(functionNames.contains('tan'), isTrue);
      expect(functionNames.contains('asin'), isTrue);
      expect(functionNames.contains('acos'), isTrue);
      expect(functionNames.contains('atan'), isTrue);

      // Utility functions
      expect(functionNames.contains('abs'), isTrue);
      expect(functionNames.contains('ceil'), isTrue);
      expect(functionNames.contains('floor'), isTrue);
      expect(functionNames.contains('round'), isTrue);
      expect(functionNames.contains('trunc'), isTrue);
      expect(functionNames.contains('max'), isTrue);
      expect(functionNames.contains('min'), isTrue);
    });

    test('math module has correct total number of functions', () {
      // Should have 31 functions total (all math functions including aliases)
      expect(math.definitions.length, equals(31));
    });

    test('math module exports all definitions', () {
      expect(math.exports.length, equals(math.definitions.length));
    });
  });
}
