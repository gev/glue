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
  group('Glue.Lib.Math.Logarithmic.Lg (lg - base 10)', () {
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

    test('fails with too few arguments', () async {
      final result = await runCode('(lg)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(lg 10 20)');
      expect(result.isLeft, isTrue);
    });
  });
}
