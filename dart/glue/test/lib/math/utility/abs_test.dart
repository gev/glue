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
  group('Glue.Lib.Math.Utility.Abs (abs)', () {
    test('abs with positive integer returns same integer', () async {
      final result = await runCode('(abs 5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(5))),
      );
    });

    test('abs with negative integer returns positive', () async {
      final result = await runCode('(abs -5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(5))),
      );
    });

    test('abs with positive float returns same float', () async {
      final result = await runCode('(abs 3.5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(3.5, 0.0001)),
      );
    });

    test('abs with negative float returns positive', () async {
      final result = await runCode('(abs -3.5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(3.5, 0.0001)),
      );
    });

    test('fails with wrong argument types', () async {
      final result = await runCode('(abs "hello")');
      expect(result.isLeft, isTrue);
    });

    test('fails with too few arguments', () async {
      final result = await runCode('(abs)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(abs 1 2)');
      expect(result.isLeft, isTrue);
    });
  });
}
