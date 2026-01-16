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
  group('Glue.Lib.Math.Arithmetic.Mul (*)', () {
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

    test('fails with wrong argument types', () async {
      final result = await runCode('(* 5 "hello")');
      expect(result.isLeft, isTrue);
    });

    test('fails with too few arguments', () async {
      final result = await runCode('(* 5)');
      expect(result.isLeft, isTrue);
    });

    test('fails with too many arguments', () async {
      final result = await runCode('(* 1 2 3)');
      expect(result.isLeft, isTrue);
    });
  });
}
