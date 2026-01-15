import 'package:glue/src/either.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/error.dart';
import 'package:glue/src/lib/builtin.dart';
import 'package:glue/src/lib/bool.dart';
import 'package:test/test.dart';

/// Helper to run full Glue code like Haskell EvalSpec.hs
Future<Either<GlueError, Ir>> runCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match((parseError) => Left(parseError), (ast) async {
    final irTree = compile(ast);
    final env = envFromModules([
      builtin,
      bool,
    ]); // TODO: Add arithmetic when implemented
    final runtime = Runtime.initial(env);

    final evalResult = await runEval(eval(irTree), runtime);
    return evalResult.match((error) => Left(error), (value) {
      final (result, _) = value;
      return Right(result);
    });
  });
}

void main() {
  group('Glue.Eval (System Integration)', () {
    test('handles basic values', () async {
      final result = await runCode('42');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('handles basic values', () async {
      final result = await runCode('"test"');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrString('test'))),
      );
    });

    test('handles basic values', () async {
      final result = await runCode('(42)');
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrInteger(42)]))),
      );
    });

    test('handles basic values', () async {
      final result = await runCode('((42))');
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

    // test('handles basic values', () async {
    //   final result = await runCode('(+ 0 42)');
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrInteger(42))),
    //   );
    // });

    // test('handles basic values', () async {
    //   final result = await runCode('((+ 0 42))');
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrInteger(42)]))),
    //   );
    // });

    // test('handles basic values', () async {
    //   final result = await runCode('(== (+ 1 1) (+ 1 1))');
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    // });

    // test('handles basic values', () async {
    //   final result = await runCode('(== (+ 1 1) ((+ 1 1)))');
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    test('executes (def)', () async {
      final code = '((def x 1) x)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(1)]))),
      );
    });

    // test('should this work?', () async {
    //   final code = '((def x 1) (def y 2) (+ x y))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) =>
    //         expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(3)]))),
    //   );
    // });

    test('executes (def)', () async {
      final code = '((def x (1)) x)';
      final result = await runCode(code);
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

    test('executes (def)', () async {
      final code = '(1 ((def x 1) x))';
      final result = await runCode(code);
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

    test('executes (def) and (set) chain', () async {
      final code = '((def x 1) (set x 2) x)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) =>
            expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(2)]))),
      );
    });

    test('implements full closures (Lexical Shadowing)', () async {
      final code = '(((lambda (x) (lambda (y) x)) 100) 1)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test(
      'checks that (def) inside (lambda) doesn\'t corrupt global scope',
      () async {
        final code = '((def x 1) ((lambda () (def x 2))) x)';
        final result = await runCode(code);
        result.match(
          (error) => fail('Should not be left: $error'),
          (value) =>
              expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(1)]))),
        );
      },
    );

    test('handles property access on property lists', () async {
      final code = '((lambda (obj) obj.foo) (:foo 42))';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    test('handles nested property access', () async {
      final code = '((def foo (:x (:y (:z 1)))) foo.x foo.x.y foo.x.y.z)';
      final result = await runCode(code);
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
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    // test('user-defined function partial application (currying)', () async {
    //   final code = '((def add (lambda (x y) (+ x y))) ((add 5) 3))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrVoid(), IrInteger(8)]))),
    //   );
    // });

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

    // test('currying works with multiple levels', () async {
    //   final code = '((def add (lambda (x y z) (+ x (+ y z)))) (((add 1) 2) 3))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrVoid(), IrInteger(6)]))),
    //   );
    // });

    test('user-defined function too many args still fails', () async {
      final code = '((def id (lambda (x) x)) (id 1 2))';
      final result = await runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('user-defined function multi-param', () async {
      final code = '((def f (lambda (a b) ((a) (b)))) (f 1 2))';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('\\ alias works like lambda (lexical shadowing)', () async {
      final code = '((( \\ (x) ( \\ (y) x)) 100) 1)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(100))),
      );
    });

    test('\\ alias works like lambda (user-defined function)', () async {
      final code = '((def id (\\ (x) x)) (id 42))';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(42)]))),
      );
    });

    // test('\\ alias works like lambda (partial application)', () async {
    //   final code = '((def add (\\ (x y) (+ x y))) (def add5 (add 5)) (add5 3))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) =>
    //         expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(8)]))),
    //   );
    // });

    test('\\ alias works like lambda (too many args)', () async {
      final code = '((def id (\\ (x) x)) (id 1 2))';
      final result = await runCode(code);
      expect(result.isLeft, isTrue);
    });

    test('\\ alias works like lambda (multi-param)', () async {
      final code = '((def f (\\ (a b) ((a) (b)))) (f 1 2))';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(2)]))),
      );
    });

    test('\\ alias works like lambda (multi-param)', () async {
      final code = '((\\ (a b) ((a) (b))) 1 2)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(2))),
      );
    });

    // test('== alias works like eq', () async {
    //   final result1 = await runCode('(== 42 42)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(== 42 43)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('\\= alias works like ne', () async {
    //   final result1 = await runCode('(!= 42 43)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(!= 42 42)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('< alias works like lt', () async {
    //   final result1 = await runCode('(< 5 10)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(< 10 5)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('<= alias works like le', () async {
    //   final result1 = await runCode('(<= 5 5)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(<= 10 5)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('> alias works like gt', () async {
    //   final result1 = await runCode('(> 10 5)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(> 5 10)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('>= alias works like ge', () async {
    //   final result1 = await runCode('(>= 5 5)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(>= 5 10)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('! alias works like not', () async {
    //   final result1 = await runCode('(! false)');
    //   result1.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(true))),
    //   );
    //   final result2 = await runCode('(! true)');
    //   result2.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrBool(false))),
    //   );
    // });

    // test('literal lists evaluate expressions', () async {
    //   final code = '((+ 1 2) (* 3 4))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrInteger(3), IrInteger(12)]))),
    //   );
    // });

    // test('literal objects evaluate values', () async {
    //   final code = '(:x (+ 1 2) :y (* 3 4))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(
    //       value,
    //       equals(IrObject({'x': IrInteger(3), 'y': IrInteger(12)})),
    //     ),
    //   );
    // });

    // test('dotted symbols work in function calls', () async {
    //   final code =
    //       '((def obj (:x (:y (:z (lambda (n) (+ n 10)))))) (obj.x.y.z 5))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrVoid(), IrInteger(15)]))),
    //   );
    // });

    // test('deep arithmetic composition', () async {
    //   final code = '(* (+ 1 2) (- 10 2))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrInteger(24))),
    //   );
    // });

    // test('complex arithmetic with mixed operations', () async {
    //   final code = '(/ (+ (* 3 4) 2) (- 10 3))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrFloat(2.0))),
    //   );
    // });

    // test('deep arithmetic with floats', () async {
    //   final code = '(+ (* 2.5 4.0) (/ 10.0 2.0))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrFloat(15.0))),
    //   );
    // });

    test('let creates local bindings', () async {
      final code = '(let (:x 42) x)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(42))),
      );
    });

    // test('let bindings can access outer scope', () async {
    //   final code = '((def outer 100) (let (:x outer) (+ x 1)))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrVoid(), IrInteger(101)]))),
    //   );
    // });

    test('let bindings shadow outer scope', () async {
      final code = '((def x 100) (let (:x 200) x))';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrList([IrVoid(), IrInteger(200)]))),
      );
    });

    // test('let with multiple bindings', () async {
    //   final code = '(let (:x 10 :y 20) (+ x y))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrInteger(30))),
    //   );
    // });

    test('let bindings are local', () async {
      final code = '((let (:x 42) x) x)';
      final result = await runCode(code);
      expect(result.isLeft, isTrue);
    });

    // test('arithmetic with defined functions', () async {
    //   final code =
    //       '((def add (lambda (x y) (+ x y))) (def mul (lambda (x y) (* x y))) (mul (add 3 2) (add 1 2)))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) =>
    //         expect(value, equals(IrList([IrVoid(), IrVoid(), IrInteger(15)]))),
    //   );
    // });

    // test('nested function calls with arithmetic', () async {
    //   final code = '((def calc (lambda (a b) (* (+ a b) (- a b)))) (calc 5 3))';
    //   final result = await runCode(code);
    //   result.match(
    //     (error) => fail('Should not be left: $error'),
    //     (value) => expect(value, equals(IrList([IrVoid(), IrInteger(16)]))),
    //   );
    // });

    // test(
    //   'function bodies with lists return last value (implicit sequences)',
    //   () async {
    //     final code = '((\\ (x y) (42 (+ x y))) 1 2)';
    //     final result = await runCode(code);
    //     result.match(
    //       (error) => fail('Should not be left: $error'),
    //       (value) => expect(value, equals(IrInteger(3))),
    //     );
    //   },
    // );

    test('function bodies with direct expressions work', () async {
      final code = '((\\ (x y) x) 1 2)';
      final result = await runCode(code);
      result.match(
        (error) => fail('Should not be left: $error'),
        (value) => expect(value, equals(IrInteger(1))),
      );
    });

    //     test('function bodies with single-element lists work', () async {
    //       final code = '((\\ (x y) ((+ x y))) 1 2)';
    //       final result = await runCode(code);
    //       result.match(
    //         (error) => fail('Should not be left: $error'),
    //         (value) => expect(value, equals(IrInteger(3))),
    //       );
    //     });
  });
}
