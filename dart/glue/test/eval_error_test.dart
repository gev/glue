import 'package:glue/src/eval/error.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:test/test.dart';

void main() {
  group('Eval Error System', () {
    test('Context typedef', () {
      final Context context = ['outer', 'inner'];
      expect(context, equals(['outer', 'inner']));
    });

    test('EvalError creation and equality', () {
      final exception = unboundVariable('testVar');
      final error1 = EvalError(['main', 'helper'], exception);
      final error2 = EvalError(['main', 'helper'], exception);

      expect(error1.context, equals(['main', 'helper']));
      expect(error1.exception, equals(exception));
      expect(error1, equals(error2));
    });

    test('EvalError toString', () {
      final exception = unboundVariable('testVar');
      final error = EvalError(['main', 'helper'], exception);

      expect(error.toString(), contains('main'));
      expect(error.toString(), contains('helper'));
      expect(error.toString(), contains('unbound-variable'));
    });

    test('prettyShow with empty context', () {
      final exception = notCallableObject();
      final error = EvalError([], exception);

      final result = prettyShow(error);
      expect(result, contains('not-callable-object'));
      expect(result, isNot(contains('->')));
    });

    test('prettyShow with context', () {
      final exception = unboundVariable('missingVar');
      final error = EvalError(['main', 'process', 'lookup'], exception);

      final result = prettyShow(error);
      expect(result, contains('lookup -> process -> main'));
      expect(result, contains('unbound-variable'));
      expect(result, contains('missingVar'));
    });

    test('prettyShow context reversal', () {
      final exception = RuntimeException('test-error', IrString('details'));
      final error = EvalError(['first', 'second', 'third'], exception);

      final result = prettyShow(error);
      // Should show: third -> second -> first: Runtime Exception: test-error. Some(IrString(details))
      expect(result, contains('third -> second -> first'));
      expect(result, contains('test-error'));
    });

    test('EvalError with different contexts are not equal', () {
      final exception = unboundVariable('x');
      final error1 = EvalError(['func1'], exception);
      final error2 = EvalError(['func2'], exception);

      expect(error1, isNot(equals(error2)));
    });

    test('EvalError with different exceptions are not equal', () {
      final error1 = EvalError(['func'], unboundVariable('x'));
      final error2 = EvalError(['func'], notCallableObject());

      expect(error1, isNot(equals(error2)));
    });
  });
}
