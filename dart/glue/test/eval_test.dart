import 'package:glue/compile.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/bool.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:glue/src/lib/math/arithmetic.dart';
import 'package:glue/src/lib/math/const.dart';
import 'package:glue/src/lib/math/trigonometric.dart';
import 'package:glue/src/lib/math/utility.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/parse.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell EvalSpec.hs
Either<GlueError, Ir> runCode(String input) {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) {
    final irTree = compile(ast);
    final env = envFromModules([
      builtinModule,
      boolModule,
      constModule,
      arithmeticModule,
      trigonometricModule,
      utilityModule,
    ]); // All math submodules loaded
    final runtime = Runtime.initial(env);
    final evalResult = runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Eval (System Integration)', () {
    test('handles basic values', () {
      final result = runCode('42');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('handles basic values', () {
      final result = runCode('"test"');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('test'))),
      );
    });

    test('handles basic values', () {
      final result = runCode('(42)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('handles basic values', () {
      final result = runCode('((42))');
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

    test('handles basic arithmetic', () {
      final result = runCode('(+ 0 42)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('handles math constants', () {
      final result1 = runCode('pi');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(3.14159, 0.0001)),
      );

      final result2 = runCode('e');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(2.71828, 0.0001)),
      );
    });

    test('handles trigonometric functions', () {
      final result = runCode('(sin 0)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect((value as IrFloat).value, closeTo(0.0, 0.0001)),
      );
    });

    test('handles utility functions', () {
      final result = runCode('(abs -5)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(5))),
      );
    });

    test('handles basic values', () {
      final result = runCode('((+ 0 42))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('handles basic values', () {
      final result = runCode('(== (+ 1 1) (+ 1 1))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
    });

    test('handles basic values', () {
      final result = runCode('(== (+ 1 1) ((+ 1 1)))');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('executes (def)', () {
      final code = '((def x 1) x)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(1)]))),
      );
    });

    test('should this work?', () {
      final code = '((def x 1) (def y 2) (+ x y))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(3)]))),
      );
    });

    test('executes (def)', () {
      final code = '((def x (1)) x)';
      final result = runCode(code);
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

    test('executes (def)', () {
      final code = '(1 ((def x 1) x))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrInteger(1),
              IrList([IrVoid(), IrInteger(1)]),
            ]),
          ),
        ),
      );
    });

    test('implements full closures (Lexical Shadowing)', () {
      final code = '(((lambda (x) (lambda (y) x)) 100) 1)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test('checks that (def) inside (lambda) doesn\'t corrupt global scope', () {
      final code = '((def x 1) ((lambda () (def x 2))) x)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(1)]))),
      );
    });

    test('handles property access on property lists', () {
      final code = '((lambda (obj) obj.foo) (:foo 42))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('handles nested property access', () {
      final code = '((def foo (:x (:y (:z 1)))) foo.x foo.x.y foo.x.y.z)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(
            IrList([
              IrVoid(),
              IrObject({
                'y': IrObject({'z': IrInteger(1)}),
              }),
              IrObject({'z': IrInteger(1)}),
              IrInteger(1),
            ]),
          ),
        ),
      );
    });

    test('fails when calling non-existent function', () {
      final result = runCode('(non-existent 1 2)');
      expect(result.isLeft, isTrue);
    });

    test('partial application returns closure', () {
      final result = runCode('((lambda (a b) a) 1)');
      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value, isA<IrClosure>());
        final closure = value as IrClosure;
        expect(closure.params, equals(['b']));
      });
    });

    test('user-defined function', () {
      final code = '((def id (lambda (x) x)) (id 42))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    test('user-defined function with quoted arg', () {
      final code = '((def id (lambda (x) x)) (id \'foo))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrSymbol('foo')]))),
      );
    });

    test('object\'s field function with quoted arg', () {
      final code = '((def o (:id (lambda (x) x))) (o.id \'foo))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrSymbol('foo')]))),
      );
    });

    test('function definition sugar syntax', () {
      final code = '((def (foo x) x) (foo 42))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    test('user-defined function partial application (currying)', () {
      final code = '((def add (lambda (x y) (+ x y))) ((add 5) 3))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(8)]))),
      );
    });

    test('user-defined function returns closure on partial application', () {
      final code = '((def add (lambda (x y) (+ x y))) (add 5))';
      final result = runCode(code);
      result.match((error) => fail('Should not be left: $error'), (value) {
        expect(value, isA<IrList>());
        final list = value as IrList;
        expect(list.elements[0], equals(IrVoid()));
        expect(list.elements[1], isA<IrClosure>());
        final closure = list.elements[1] as IrClosure;
        expect(closure.params, equals(['y']));
      });
    });

    test('currying works with multiple levels', () {
      final code = '((def add (lambda (x y z) (+ x (+ y z)))) (((add 1) 2) 3))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(6)]))),
      );
    });

    test('user-defined function too many args still fails', () {
      final code = '((def id (lambda (x) x)) (id 1 2))';
      final result = runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('user-defined function multi-param', () {
      final code = '((def f (lambda (a b) (a) (b))) (f 1 2))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('\\ alias works like lambda (lexical shadowing)', () {
      final code = '((( \\ (x) ( \\ (y) x)) 100) 1)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test('\\ alias works like lambda (user-defined function)', () {
      final code = '((def id (\\ (x) x)) (id 42))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    test('\\ alias works like lambda (partial application)', () {
      final code = '((def add (\\ (x y) (+ x y))) (def add5 (add 5)) (add5 3))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(8)]))),
      );
    });

    test('\\ alias works like lambda (too many args)', () {
      final code = '((def id (\\ (x) x)) (id 1 2))';
      final result = runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('\\ alias works like lambda (multi-param)', () {
      final code = '((def f (\\ (a b) (a) (b))) (f 1 2))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('\\ alias works like lambda (multi-param)', () {
      final code = '((\\ (a b) (a) (b)) 1 2)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(2))),
      );
    });

    test('== alias works like eq', () {
      final result1 = runCode('(== 42 42)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(== 42 43)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('!= alias works like ne', () {
      final result1 = runCode('(!= 42 43)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(!= 42 42)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('< alias works like lt', () {
      final result1 = runCode('(< 5 10)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(< 10 5)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('<= alias works like le', () {
      final result1 = runCode('(<= 5 5)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(<= 10 5)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('> alias works like gt', () {
      final result1 = runCode('(> 10 5)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(> 5 10)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('>= alias works like ge', () {
      final result1 = runCode('(>= 5 5)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(>= 5 10)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('! alias works like not', () {
      final result1 = runCode('(! false)');
      result1.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(true))),
      );
      final result2 = runCode('(! true)');
      result2.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrBool(false))),
      );
    });

    test('literal lists evaluate expressions', () {
      final code = '((+ 1 2) (* 3 4))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(3), IrInteger(12)]))),
      );
    });

    test('literal objects evaluate values', () {
      final code = '(:x (+ 1 2) :y (* 3 4))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(
          value,
          equals(IrObject({'x': IrInteger(3), 'y': IrInteger(12)})),
        ),
      );
    });

    test('dotted symbols work in function calls', () {
      final code =
          '((def obj (:x (:y (:z (lambda (n) (+ n 10)))))) (obj.x.y.z 5))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(15)]))),
      );
    });

    test('deep arithmetic composition', () {
      final code = '(* (+ 1 2) (- 10 2))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(24))),
      );
    });

    test('complex arithmetic with mixed operations', () {
      final code = '(/ (+ (* 3 4) 2) (- 10 3))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrFloat(2.0))),
      );
    });

    test('deep arithmetic with floats', () {
      final code = '(+ (* 2.5 4.0) (/ 10.0 2.0))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrFloat(15.0))),
      );
    });

    test('let creates local bindings', () {
      final code = '(let (def x 42) x)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('let bindings can access outer scope', () {
      final code = '((def outer 100) (let (def x outer) (+ x 1)))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(101)]))),
      );
    });

    test('let bindings shadow outer scope', () {
      final code = '((def x 100) (let (def x 200) x))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(200)]))),
      );
    });

    test('let with multiple bindings', () {
      final code = '(let (def x 10) (def y 20) (+ x y))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(30))),
      );
    });

    test('let bindings are local', () {
      final code = '((let (def x 42) x) x)';
      final result = runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('arithmetic with defined functions', () {
      final code =
          '((def add (lambda (x y) (+ x y))) (def mul (lambda (x y) (* x y))) (mul (add 3 2) (add 1 2)))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(15)]))),
      );
    });

    test('nested function calls with arithmetic', () {
      final code = '((def calc (lambda (a b) (* (+ a b) (- a b)))) (calc 5 3))';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(16)]))),
      );
    });

    test(
      'function bodies with lists return last value (implicit sequences)',
      () {
        final code = '((\\ (x y) 42 (+ x y)) 1 2)';
        final result = runCode(code);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(3))),
        );
      },
    );

    test('function bodies with direct expressions work', () {
      final code = '((\\ (x y) x) 1 2)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(1))),
      );
    });

    test('function bodies with single-element lists work', () {
      final code = '((\\ (x y) (+ x y)) 1 2)';
      final result = runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(3))),
      );
    });

    group('NativeFunc Partial Application', () {
      test('native function single arg returns function', () {
        final result = runCode('((+) 5)');
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, isA<IrNativeFunc>()),
        );
      });

      test('native function double args return result', () {
        final result1 = runCode('((+) 5 3)');
        result1.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(8))),
        );

        final result2 = runCode('((-) 10 4)');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(6))),
        );

        final result3 = runCode('((*) 3 7)');
        result3.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(21))),
        );

        final result4 = runCode('((/) 15 3)');
        result4.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrFloat(5.0))),
        );
      });

      test('native function triple args fail', () {
        final result1 = runCode('((+) 5 3 1)');
        expect(result1.isLeft, isTrue);

        final result2 = runCode('((-) 10 4 2)');
        expect(result2.isLeft, isTrue);
      });

      test('nested partial application works', () {
        final result1 = runCode('(((+) 5) 3)');
        result1.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(8))),
        );

        final result2 = runCode('(((+) ((-) 10 2)) 3)');
        result2.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(11))),
        );

        final result3 = runCode('(((*) ((+) 2 3)) 4)');
        result3.match(
          (error) => fail('Should not be left: $error'),
          (value) => expect(value, equals(IrInteger(20))),
        );
      });
    });
  });
}
