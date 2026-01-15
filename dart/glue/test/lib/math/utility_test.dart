import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/utility/utility.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      utility,
    ]); // Load only utility module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Utility (Test utility functions)', () {
    group('Absolute value (abs)', () {
      test('abs with positive integer returns same integer', () async {
        final result = await runCode('(abs 5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(5))),
        );
      });

      test('abs with negative integer returns positive integer', () async {
        final result = await runCode('(abs -3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(3))),
        );
      });

      test('abs with positive float returns same float', () async {
        final result = await runCode('(abs 3.14)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.14, 0.0001)),
        );
      });

      test('abs with negative float returns positive float', () async {
        final result = await runCode('(abs -2.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(2.5, 0.0001)),
        );
      });
    });

    group('Rounding functions (floor, ceil, round, trunc)', () {
      test('floor with positive float returns integer', () async {
        final result = await runCode('(floor 3.7)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(3))),
        );
      });

      test('floor with negative float returns integer', () async {
        final result = await runCode('(floor -2.3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(-3))),
        );
      });

      test('ceil with positive float returns integer', () async {
        final result = await runCode('(ceil 3.1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(4))),
        );
      });

      test('ceil with negative float returns integer', () async {
        final result = await runCode('(ceil -2.9)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(-2))),
        );
      });

      test('round with positive float returns integer', () async {
        final result = await runCode('(round 3.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(4))),
        );
      });

      test('trunc with positive float returns integer', () async {
        final result = await runCode('(trunc 3.9)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(3))),
        );
      });

      test('trunc with negative float returns integer', () async {
        final result = await runCode('(trunc -2.1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(-2))),
        );
      });

      test(
        'rounding functions with integer input return same integer',
        () async {
          final result = await runCode('(floor 5)');
          result.match(
            (error) => fail('Should not be left: $error'),
            (value) => expect(value, equals(IrInteger(5))),
          );
        },
      );
    });

    group('Min/max functions', () {
      test('max with two integers returns integer', () async {
        final result = await runCode('(max 5 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(5))),
        );
      });

      test('max with two floats returns float', () async {
        final result = await runCode('(max 3.5 4.2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(4.2, 0.0001)),
        );
      });

      test('max with mixed types returns float', () async {
        final result = await runCode('(max 5 3.2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(5.0, 0.0001)),
        );
      });

      test('min with two integers returns integer', () async {
        final result = await runCode('(min 5 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(3))),
        );
      });

      test('min with two floats returns float', () async {
        final result = await runCode('(min 3.5 4.2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.5, 0.0001)),
        );
      });

      test('min with mixed types returns float', () async {
        final result = await runCode('(min 5 3.2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.2, 0.0001)),
        );
      });
    });

    group('Error handling', () {
      test('single-argument functions fail with too few arguments', () async {
        final result = await runCode('(abs)');
        expect(result.isLeft, isTrue);
      });

      test('single-argument functions fail with too many arguments', () async {
        final result = await runCode('(floor 1 2)');
        expect(result.isLeft, isTrue);
      });

      test('two-argument functions fail with too few arguments', () async {
        final result = await runCode('(max 5)');
        expect(result.isLeft, isTrue);
      });

      test('two-argument functions fail with too many arguments', () async {
        final result = await runCode('(min 1 2 3)');
        expect(result.isLeft, isTrue);
      });

      test('all functions fail with non-numeric arguments', () async {
        final result = await runCode('(abs "hello")');
        expect(result.isLeft, isTrue);
      });
    });
  });
}
