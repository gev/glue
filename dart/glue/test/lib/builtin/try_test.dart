import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/parse.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell tests
Either<GlueError, Ir?> runCode(String input) {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) {
    final irTree = compile(ast);
    final env = envFromModules([
      builtinModule,
    ]); // TODO: Add arithmetic module when implemented
    final runtime = Runtime.initial(env);

    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Try Special Form', () {
    test('catches exception and calls handler with payload', () {
      const code =
          '(try (error test-error (:msg "hello")) (catch test-error (lambda (err) err.msg)))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('hello'))),
      );
    });

    test('returns normal value when no exception', () {
      const code = '(try 42 (catch any-error (lambda (err) "caught")))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('re-throws unmatched exception', () {
      const code =
          '(try (error test-error (:msg "hello")) (catch other-error (lambda (err) err.msg)))';
      final result = runCode(code);
      expect(result.isLeft, isTrue); // Should be an error
    });

    test('works with symbol catch names', () {
      const code =
          '(try (error test-error (:msg "hello")) (catch test-error (lambda (err) err.msg)))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('hello'))),
      );
    });

    // test('handler can be any callable', () {
    //   const code =
    //       '(try (error test-error (:val 123)) (catch test-error (lambda (err) (+ err.val 1))))';
    //   final result = runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrInteger(124))),
    //   );
    // });

    test('multiple catch clauses work', () {
      const code =
          '(try (error second-error (:msg "second")) (catch first-error (lambda (err) "first")) (catch second-error (lambda (err) err.msg)))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('second'))),
      );
    });

    test('first matching catch is used', () {
      const code =
          '(try (error test-error (:msg "caught")) (catch test-error (lambda (err) err.msg)) (catch test-error (lambda (err) "second")))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('caught'))),
      );
    });

    test('fails to catch when using string instead of symbol', () {
      const code =
          '(try (error test-error (:msg "hello")) (catch "test-error" (lambda (err) err.msg)))';
      final result = runCode(code);
      expect(
        result.isLeft,
        isTrue,
      ); // Should be an error since string can't match symbol
    });
  });
}
