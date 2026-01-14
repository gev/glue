import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Future<Either<String, Ir?>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left('Parse error: $parseError'), (
    ast,
  ) async {
    final irTree = compile(ast);
    final env = envFromModules([
      builtin,
    ]); // TODO: Add arithmetic module when implemented
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left('Eval error: $error'), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Try Special Form', () {
    test('catches exception and calls handler with payload', () async {
      const code =
          '(try (error "test-error" "hello") (catch "test-error" (lambda (err) err)))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('hello'))),
      );
    });

    test('returns normal value when no exception', () async {
      const code = '(try 42 (catch "any-error" (lambda (err) "caught")))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('re-throws unmatched exception', () async {
      const code =
          '(try (error "test-error" "hello") (catch "other-error" (lambda (err) err)))';
      final result = await runCode(code);
      expect(result.isLeft, isTrue); // Should be an error
    });

    test('works with symbol catch names', () async {
      const code =
          '(try (error test-error "hello") (catch test-error (lambda (err) err)))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('hello'))),
      );
    });

    test('handler can be any callable', () async {
      const code =
          '(try (error test-error 123) (catch test-error (lambda (err) err)))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(123))),
      );
    });

    test('multiple catch clauses work', () async {
      const code =
          '(try (error "second-error" "second") (catch "first-error" (lambda (err) "first")) (catch "second-error" (lambda (err) err)))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('second'))),
      );
    });

    test('first matching catch is used', () async {
      const code =
          '(try (error test-error "caught") (catch test-error (lambda (err) err)) (catch test-error (lambda (err) "second")))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('caught'))),
      );
    });
  });
}
