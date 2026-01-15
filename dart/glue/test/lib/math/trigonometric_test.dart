import 'dart:math' as math;

import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/trigonometric/trigonometric.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      trigonometric,
    ]); // Load only trigonometric module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Trigonometric (Test trigonometric functions)', () {
    group('Direct trigonometric functions (radians)', () {
      test('sin with common angles', () async {
        final result = await runCode('(sin 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );

        final result2 = await runCode('(sin ${math.pi / 2})');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('cos with common angles', () async {
        final result = await runCode('(cos 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );

        final result2 = await runCode('(cos ${math.pi})');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(-1.0, 0.0001)),
        );
      });

      test('tan with common angles', () async {
        final result = await runCode('(tan 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );
      });

      test('functions handle integer input', () async {
        final result = await runCode('(sin 1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.sin(1.0), 0.0001)),
        );
      });
    });

    group('Inverse trigonometric functions (return radians)', () {
      test('asin with valid inputs', () async {
        final result = await runCode('(asin 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );

        final result2 = await runCode('(asin 1)');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.pi / 2, 0.0001)),
        );
      });

      test('acos with valid inputs', () async {
        final result = await runCode('(acos 1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );

        final result2 = await runCode('(acos 0)');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.pi / 2, 0.0001)),
        );
      });

      test('atan with valid inputs', () async {
        final result = await runCode('(atan 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );

        final result2 = await runCode('(atan 1)');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.pi / 4, 0.0001)),
        );
      });
    });

    group('Error handling', () {
      test('sin fails with too few arguments', () async {
        final result = await runCode('(sin)');
        expect(result.isLeft, isTrue);
      });

      test('sin fails with too many arguments', () async {
        final result = await runCode('(sin 1 2)');
        expect(result.isLeft, isTrue);
      });

      test('functions fail with non-numeric arguments', () async {
        final result = await runCode('(sin "hello")');
        expect(result.isLeft, isTrue);
      });

      test('asin returns NaN for out-of-domain values', () async {
        final result = await runCode('(asin 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value.isNaN, isTrue),
        );
      });

      test('acos returns NaN for out-of-domain values', () async {
        final result = await runCode('(acos 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value.isNaN, isTrue),
        );
      });
    });

    group('Type handling', () {
      test('all functions return Float for Integer input', () async {
        final result = await runCode('(sin 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, isA<IrFloat>()),
        );
      });

      test('all functions return Float for Float input', () async {
        final result = await runCode('(cos 0.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, isA<IrFloat>()),
        );
      });
    });
  });
}
