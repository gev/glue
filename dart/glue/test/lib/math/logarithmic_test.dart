import 'dart:math' as math;

import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/logarithmic/logarithmic.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      logarithmic,
    ]); // Load only logarithmic module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Logarithmic (Test logarithmic functions)', () {
    group('Common logarithm (lg - base 10)', () {
      test('lg with integer returns float', () async {
        final result = await runCode('(lg 100)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(2.0, 0.0001)),
        );
      });

      test('lg with float returns float', () async {
        final result = await runCode('(lg 10.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('lg with 1 returns 0', () async {
        final result = await runCode('(lg 1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(lg "hello")');
        expect(result.isLeft, isTrue);
      });
    });

    group('Natural logarithm (ln - base e)', () {
      test('ln with integer returns float', () async {
        final result = await runCode('(ln 1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
        );
      });

      test('ln with float returns float', () async {
        final result = await runCode('(ln 2.718281828)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('ln with e returns 1', () async {
        final result = await runCode('(ln ${math.e})');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(ln true)');
        expect(result.isLeft, isTrue);
      });
    });

    group('Arbitrary base logarithm (log)', () {
      test('log with integer base and value returns float', () async {
        final result = await runCode('(log 8 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.0, 0.0001)),
        );
      });

      test('log with float base and value returns float', () async {
        final result = await runCode('(log 9.0 3.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(2.0, 0.0001)),
        );
      });

      test('log with mixed types returns float', () async {
        final result = await runCode('(log 16 2.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(4.0, 0.0001)),
        );
      });

      test('log base 10 equals lg', () async {
        final result1 = await runCode('(log 100 10)');
        final result2 = await runCode('(lg 100)');

        result1.match(
          (error) => fail('Should not be left: $error'),
          (value1) => result2.match(
            (error) => fail('Should not be left: $error'),
            (value2) => expect(
              (value1 as IrFloat).value,
              closeTo((value2 as IrFloat).value, 0.0001),
            ),
          ),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(log 8 "hello")');
        expect(result.isLeft, isTrue);
      });
    });

    group('Error handling', () {
      test('lg fails with too few arguments', () async {
        final result = await runCode('(lg)');
        expect(result.isLeft, isTrue);
      });

      test('lg fails with too many arguments', () async {
        final result = await runCode('(lg 10 20)');
        expect(result.isLeft, isTrue);
      });

      test('ln fails with too few arguments', () async {
        final result = await runCode('(ln)');
        expect(result.isLeft, isTrue);
      });

      test('ln fails with too many arguments', () async {
        final result = await runCode('(ln 2.718 1)');
        expect(result.isLeft, isTrue);
      });

      test('log fails with too few arguments', () async {
        final result = await runCode('(log 8)');
        expect(result.isLeft, isTrue);
      });

      test('log fails with too many arguments', () async {
        final result = await runCode('(log 8 2 10)');
        expect(result.isLeft, isTrue);
      });
    });
  });
}
