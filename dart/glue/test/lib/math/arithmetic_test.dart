import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/math/arithmetic/arithmetic.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      arithmetic,
    ]); // Load only arithmetic module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.Math.Arithmetic (Test arithmetic operations)', () {
    group('Addition (+)', () {
      test('adds integers correctly', () async {
        final result = await runCode('(+ 5 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(8))),
        );
      });

      test('adds floats correctly', () async {
        final result = await runCode('(+ 3.5 2.1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(5.6, 0.0001)),
        );
      });

      test('converts mixed types to float', () async {
        final result = await runCode('(+ 5 2.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(7.5, 0.0001)),
        );
      });

      test('fails with wrong argument types', () async {
        final result = await runCode('(+ 5 "hello")');
        expect(result.isLeft, isTrue);
      });
    });

    group('Subtraction (-)', () {
      test('subtracts integers correctly', () async {
        final result = await runCode('(- 10 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(7))),
        );
      });

      test('subtracts floats correctly', () async {
        final result = await runCode('(- 5.5 2.1)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.4, 0.0001)),
        );
      });

      test('converts mixed types to float', () async {
        final result = await runCode('(- 8 2.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(5.5, 0.0001)),
        );
      });
    });

    group('Multiplication (*)', () {
      test('multiplies integers correctly', () async {
        final result = await runCode('(* 6 7)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(42))),
        );
      });

      test('multiplies floats correctly', () async {
        final result = await runCode('(* 3.5 2.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(7.0, 0.0001)),
        );
      });

      test('converts mixed types to float', () async {
        final result = await runCode('(* 4 2.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(10.0, 0.0001)),
        );
      });
    });

    group('Division (/)', () {
      test('divides integers and returns float', () async {
        final result = await runCode('(/ 10 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(5.0, 0.0001)),
        );
      });

      test('divides floats correctly', () async {
        final result = await runCode('(/ 7.5 2.5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(3.0, 0.0001)),
        );
      });

      test('converts mixed types to float', () async {
        final result = await runCode('(/ 9 2)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(4.5, 0.0001)),
        );
      });
    });

    group('Modulo (%)', () {
      test('modulo with integers returns integer', () async {
        final result = await runCode('(% 10 3)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(1))),
        );
      });

      test('modulo with floats returns float', () async {
        final result = await runCode('(% 10.0 3.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('modulo with mixed types returns float', () async {
        final result = await runCode('(% 10 3.0)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect((value as IrFloat).value, closeTo(1.0, 0.0001)),
        );
      });

      test('fails with division by zero', () async {
        final result = await runCode('(% 10 0)');
        expect(result.isLeft, isTrue);
      });
    });

    group('Error handling', () {
      test('fails with too few arguments', () async {
        final result = await runCode('(+ 5)');
        expect(result.isLeft, isTrue);
      });

      test('fails with too many arguments', () async {
        final result = await runCode('(+ 1 2 3)');
        expect(result.isLeft, isTrue);
      });

      test('fails with non-numeric arguments', () async {
        final result = await runCode('(+ 5 true)');
        expect(result.isLeft, isTrue);
      });
    });
  });
}
