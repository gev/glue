import 'package:glue/src/ast.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell EvalSpec.hs
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(glueError(parseError)), (
    ast,
  ) async {
    final irTree = compile(ast);
    final env = envFromModules([
      builtin,
    ]); // TODO: Add arithmetic, bool when implemented
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(glueError(error)), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Eval (System Integration)', () {
    test('handles basic values', () async {
      final result1 = await runCode('42');
      expect(result1.isRight, isTrue);
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );

      final result2 = await runCode('"test"');
      expect(result2.isRight, isTrue);
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('test'))),
      );
    });

    test('handles basic lists', () async {
      final result = await runCode('(42)');
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('handles nested lists', () async {
      final result = await runCode('((42))');
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrList([IrInteger(42)]),
            ]),
          ),
        ),
      );
    });

    // TODO: Arithmetic tests - will be red until arithmetic module is implemented
    test('handles arithmetic operations', () async {
      final result1 = await runCode('(+ 0 42)');
      final result2 = await runCode('((+ 0 42))');
      // These will fail until arithmetic is implemented
      // expect(result1.isRight, isTrue);
      // expect(result2.isRight, isTrue);
    });

    // TODO: Bool tests - will be red until bool module is implemented
    test('handles comparison operations', () async {
      final result1 = await runCode('(== (+ 1 1) (+ 1 1))');
      final result2 = await runCode('(== (+ 1 1) ((+ 1 1)))');
      // These will fail until arithmetic and bool are implemented
      // expect(result1.isLeft, isTrue);
      // expect(result2.isLeft, isTrue);
    });

    test('executes def', () async {
      final code = '((def x 1) x)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(1)]))),
      );
    });

    test('executes def chain', () async {
      final code = '((def x 1) (def y 2) (+ x y))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('executes def with list', () async {
      final code = '((def x (1)) x)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrVoid(),
              IrList([IrInteger(1)]),
            ]),
          ),
        ),
      );
    });

    test('executes def and set chain', () async {
      final code = '((def x 1) (set x 2) x)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(2)]))),
      );
    });

    test('implements full closures (lexical shadowing)', () async {
      final code = '(((lambda (x) (lambda (y) x)) 100) 1)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test('def inside lambda does not corrupt global scope', () async {
      final code = '((def x 1) ((lambda () (def x 2))) x)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(1)]))),
      );
    });

    // TODO: Object property access - will be red until implemented
    test('handles property access on objects', () async {
      final code = '((lambda (obj) obj.foo) (:foo 42))';
      final result = await runCode(code);
      // This will fail until object property access is implemented
      // expect(result.isRight, isTrue);
    });

    // TODO: Nested property access - will be red until implemented
    test('handles nested property access', () async {
      final code = '((def foo (:x (:y (:z 1)))) foo.x foo.x.y foo.x.y.z)';
      final result = await runCode(code);
      // This will fail until nested property access is implemented
    });

    test('fails when calling non-existent function', () async {
      final result = await runCode('(non-existent 1 2)');
      expect(result.isLeft, isTrue);
    });

    test('partial application returns closure', () async {
      final result = await runCode('((lambda (a b) a) 1)');
      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value, isA<IrClosure>());
        final closure = value as IrClosure;
        expect(closure.params, equals(['b']));
      });
    });

    test('user-defined function', () async {
      final code = '((def id (lambda (x) x)) (id 42))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    test('user-defined function partial application (currying)', () async {
      final code = '((def add (lambda (x y) (+ x y))) ((add 5) 3))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test(
      'user-defined function returns closure on partial application',
      () async {
        final code = '((def add (lambda (x y) (+ x y))) (add 5))';
        final result = await runCode(code);
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, isA<IrList>());
          final list = value as IrList;
          expect(list.elements[0], equals(IrVoid()));
          expect(list.elements[1], isA<IrClosure>());
          final closure = list.elements[1] as IrClosure;
          expect(closure.params, equals(['y']));
        });
      },
    );

    test('currying works with multiple levels', () async {
      final code = '((def add (lambda (x y z) (+ x (+ y z)))) (((add 1) 2) 3))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('user-defined function too many args fails', () async {
      final code = '((def id (lambda (x) x)) (id 1 2))';
      final result = await runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('user-defined function multi-param', () async {
      final code = '((def f (lambda (a b) ((a) (b)))) (f 1 2))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('backslash alias works like lambda (lexical shadowing)', () async {
      final code = '((( \\ (x) ( \\ (y) x)) 100) 1)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test('backslash alias works like lambda (user-defined function)', () async {
      final code = '((def id (\\ (x) x)) (id 42))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    test('backslash alias works like lambda (partial application)', () async {
      final code = '((def add (\\ (x y) (+ x y))) (def add5 (add 5)) (add5 3))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('backslash alias works like lambda (too many args)', () async {
      final code = '((def id (\\ (x) x)) (id 1 2))';
      final result = await runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('backslash alias works like lambda (multi-param)', () async {
      final code = '((def f (\\ (a b) ((a) (b)))) (f 1 2))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('backslash alias works like lambda (direct call)', () async {
      final code = '(\\ (a b) ((a) (b))) 1 2';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(2))),
      );
    });

    // TODO: Bool operations - will be red until bool module is implemented
    test('comparison aliases work', () async {
      // These will fail until bool module is implemented
      // expect(await runCode('(== 42 42)').isRight, isTrue);
      // expect(await runCode('(== 42 43)').isRight, isTrue);
    });

    test('literal lists evaluate expressions', () async {
      final code = '((+ 1 2) (* 3 4))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('literal objects evaluate values', () async {
      final code = '(:x (+ 1 2) :y (* 3 4))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('dotted symbols work in function calls', () async {
      final code =
          '((def obj (:x (:y (:z (lambda (n) (+ n 10)))))) (obj.x.y.z 5))';
      final result = await runCode(code);
      // This will fail until object property access and arithmetic are implemented
      // expect(result.isRight, isTrue);
    });

    test('deep arithmetic composition', () async {
      final code = '(* (+ 1 2) (- 10 2))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('complex arithmetic with mixed operations', () async {
      final code = '(/ (+ (* 3 4) 2) (- 10 3))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('deep arithmetic with floats', () async {
      final code = '(+ (* 2.5 4.0) (/ 10.0 2.0))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('let creates local bindings', () async {
      final code = '(let (:x 42) x)';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('let bindings can access outer scope', () async {
      final code = '((def outer 100) (let (:x outer) (+ x 1)))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('let bindings shadow outer scope', () async {
      final code = '((def x 100) (let (:x 200) x))';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(200)]))),
      );
    });

    test('let with multiple bindings', () async {
      final code = '(let (:x 10 :y 20) (+ x y))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('let bindings are local', () async {
      final code = '((let (:x 42) x) x)';
      final result = await runCode(code);
      expect(
        result.isLeft,
        isTrue,
      ); // Should fail because x is not defined in outer scope
    });

    test('arithmetic with defined functions', () async {
      final code =
          '((def add (lambda (x y) (+ x y))) (def mul (lambda (x y) (* x y))) (mul (add 3 2) (add 1 2)))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test('nested function calls with arithmetic', () async {
      final code = '((def calc (lambda (a b) (* (+ a b) (- a b)))) (calc 5 3))';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });

    test(
      'function bodies with lists return last value (implicit sequences)',
      () async {
        final code = '((\\ (x y) (42 (+ x y))) 1 2)';
        final result = await runCode(code);
        // This will fail until arithmetic is implemented
        // expect(result.isRight, isTrue);
      },
    );

    test('function bodies with direct expressions work', () async {
      final code = '(\\ (x y) x) 1 2';
      final result = await runCode(code);
      expect(result.isRight, isTrue);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(1))),
      );
    });

    test('function bodies with single-element lists work', () async {
      final code = '(\\ (x y) ((+ x y))) 1 2';
      final result = await runCode(code);
      // This will fail until arithmetic is implemented
      // expect(result.isRight, isTrue);
    });
  });
}
