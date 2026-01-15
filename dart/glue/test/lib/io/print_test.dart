import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/io.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([io]); // Load only io module for testing
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Lib.IO.Print', () {
    test('print returns void', () async {
      final result = await runCode('(print "hello")');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrVoid())),
      );
    });

    test('println returns void', () async {
      final result = await runCode('(println "hello")');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrVoid())),
      );
    });
  });
}
