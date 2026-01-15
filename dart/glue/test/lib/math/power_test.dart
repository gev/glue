import 'dart:math' as math;

import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/power/power.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([power]); // Load only power module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Power (Test power functions)', () {
    group('Exponential function (exp)', () {
      test('exp with integer returns float', () async {
        final result = await runCode('(exp 0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('exp with float returns float', () async {
        final result = await runCode('(exp 1.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(math.e, 0.0001)),
        );
      });

      test('exp with negative number', () async {
        final result = await runCode('(exp -1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(1 / math.e, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(exp "hello")');
        expect(result.isLeft, isTrue);
      });
    });

    group('Power function (pow)', () {
      test('integer base and exponent returns integer', () async {
        final result = await runCode('(pow 2 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(8))),
        );
      });

      test('integer base, float exponent returns float', () async {
        final result = await runCode('(pow 2 0.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.sqrt(2), 0.0001)),
        );
      });

      test('float base, integer exponent returns float', () async {
        final result = await runCode('(pow 1.5 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(2.25, 0.0001)),
        );
      });

      test('float base and exponent returns float', () async {
        final result = await runCode('(pow 2.0 3.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(8.0, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(pow 2 "hello")');
        expect(result.isLeft, isTrue);
      });
    });

    group('Square root function (sqrt)', () {
      test('sqrt with integer returns float', () async {
        final result = await runCode('(sqrt 4)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(2.0, 0.0001)),
        );
      });

      test('sqrt with float returns float', () async {
        final result = await runCode('(sqrt 2.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect((value as IrFloat).value, closeTo(math.sqrt(2), 0.0001)),
        );
      });

      test('sqrt with perfect square', () async {
        final result = await runCode('(sqrt 9)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.0, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(sqrt true)');
        expect(result.isLeft, isTrue);
      });
    });

    group('Error handling', () {
      test('exp fails with too few arguments', () async {
        final result = await runCode('(exp)');
        expect(result.isLeft, isTrue);
      });

      test('exp fails with too many arguments', () async {
        final result = await runCode('(exp 1 2)');
        expect(result.isLeft, isTrue);
      });

      test('pow fails with too few arguments', () async {
        final result = await runCode('(pow 2)');
        expect(result.isLeft, isTrue);
      });

      test('pow fails with too many arguments', () async {
        final result = await runCode('(pow 2 3 4)');
        expect(result.isLeft, isTrue);
      });

      test('sqrt fails with too few arguments', () async {
        final result = await runCode('(sqrt)');
        expect(result.isLeft, isTrue);
      });

      test('sqrt fails with too many arguments', () async {
        final result = await runCode('(sqrt 4 5)');
        expect(result.isLeft, isTrue);
      });
    });
  });
}
