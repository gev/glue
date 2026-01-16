import 'package:glue/src/env.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:test/test.dart';

// Test data types for host values
class TestWidget {
  final String label;
  final bool enabled;
  const TestWidget(this.label, this.enabled);

  @override
  String toString() => 'TestWidget($label, $enabled)';

  @override
  bool operator ==(Object other) =>
      other is TestWidget && other.label == label && other.enabled == enabled;

  @override
  int get hashCode => Object.hash(label, enabled);
}

class TestConnection {
  final String host;
  final int port;
  const TestConnection(this.host, this.port);

  @override
  String toString() => 'TestConnection($host, $port)';

  @override
  bool operator ==(Object other) =>
      other is TestConnection && other.host == host && other.port == port;

  @override
  int get hashCode => Object.hash(host, port);
}

void main() {
  group('HostValue system for native object integration', () {
    group('HostValue creation and extraction', () {
      test('creates host value from any typeable value', () {
        final widget = TestWidget('button', true);
        final hv = hostValue(widget);
        expect(hv, isNotNull); // Just check it creates successfully
      });

      test('extracts host value with correct type', () {
        final widget = TestWidget('submit', false);
        final hv = hostValue(widget);
        expect(extractHostValue<TestWidget>(hv), equals(widget));
      });

      test('fails to extract with wrong type', () {
        final widget = TestWidget('test', true);
        final hv = hostValue(widget);
        expect(extractHostValue<String>(hv), isNull);
      });

      test('handles different host value types', () {
        final conn = TestConnection('localhost', 5432);
        final hv = hostValue(conn);
        expect(extractHostValue<TestConnection>(hv), equals(conn));
      });

      test('round-trip: any typeable value can be stored and retrieved', () {
        final w = TestWidget('roundtrip', false);
        final hv = hostValue(w);
        expect(extractHostValue<TestWidget>(hv), equals(w));
      });
    });

    group('HostValue equality', () {
      test('host values are never equal (opaque comparison)', () {
        final hv1 = hostValue(TestWidget('a', true));
        final hv2 = hostValue(TestWidget('a', true));
        expect(hv1 == hv2, isFalse);
      });

      test('different host values are not equal', () {
        final hv1 = hostValue(TestWidget('a', true));
        final hv2 = hostValue(TestWidget('b', false));
        expect(hv1 == hv2, isFalse);
      });
    });

    group('HostValue in IR system', () {
      test('creates IR with host value', () {
        final widget = TestWidget('test', true);
        final hv = hostValue(widget);
        final ir = IrNativeValue(hv);
        expect(ir, isNotNull);
      });

      test('identifies host value IR', () {
        final hv = hostValue(TestWidget('test', true));
        final ir = IrNativeValue(hv);
        expect(isHostValue(ir), isTrue);
      });

      test('non-host IR is not identified as host value', () {
        final ir = IrInteger(42);
        expect(isHostValue(ir), isFalse);
      });

      test('extracts host value from IR', () {
        final widget = TestWidget('extract', false);
        final hv = hostValue(widget);
        final ir = IrNativeValue(hv);
        final extracted = getHostValueFromIR(ir);
        expect(extracted, isNotNull);
        expect(extractHostValue<TestWidget>(extracted!), equals(widget));
      });

      test('returns Nothing for non-host IR', () {
        final ir = IrString('not host');
        expect(getHostValueFromIR(ir), isNull);
      });
    });

    group('NativeFunc and Special constructors', () {
      test('creates NativeFunc', () {
        final nf = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(42)));
        expect(nf, isNotNull);
      });

      test('creates Special', () {
        final s = IrSpecial((List<Ir> args) => Eval.pure(IrInteger(42)));
        expect(s, isNotNull);
      });
    });

    group('NativeFunc and Special equality', () {
      test('NativeFunc aren\'t equal', () {
        final f1 = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(1)));
        final f2 = IrNativeFunc((List<Ir> args) => Eval.pure(IrInteger(2)));
        expect(f1 == f2, isFalse);
      });

      test('Special aren\'t equal', () {
        final s1 = IrSpecial((List<Ir> args) => Eval.pure(IrInteger(1)));
        final s2 = IrSpecial((List<Ir> args) => Eval.pure(IrInteger(2)));
        expect(s1 == s2, isFalse);
      });
    });

    group('IR with NativeValue', () {
      test('IR equality handles NativeValue (host values are never equal)', () {
        final hv1 = hostValue(TestWidget('ir', true));
        final hv2 = hostValue(
          TestWidget('ir', true),
        ); // Same content, different instances
        final ir1 = IrNativeValue(hv1);
        final ir2 = IrNativeValue(hv2);
        expect(ir1 == ir2, isFalse); // Host values are never equal
      });

      test('IR show displays NativeValue', () {
        final hv = hostValue(TestWidget('show', false));
        final ir = IrNativeValue(hv);
        expect(ir.toString(), contains('<host:'));
      });
    });

    group('Type safety guarantees', () {
      test('prevents incorrect type extraction at runtime', () {
        final widget = TestWidget('type', true);
        final hv = hostValue(widget);
        // This should fail at runtime, not compile time
        expect(extractHostValue<String>(hv), isNull);
      });

      test('type safety: extraction succeeds only for correct types', () {
        final w = TestWidget('type', true);
        final s = 'not a widget';
        final hv = hostValue(w);
        final widgetResult = extractHostValue<TestWidget>(hv);
        final stringResult = extractHostValue<String>(hv);
        expect(widgetResult, equals(w));
        expect(stringResult, isNull);
      });
    });

    group('Host value lifecycle', () {
      test('host values can be created from complex types', () {
        final complex = [TestWidget('a', true), TestWidget('b', false)];
        final hv = hostValue(complex);
        expect(extractHostValue<List<TestWidget>>(hv), equals(complex));
      });

      test('host values preserve nested structures', () {
        final nested = ('tuple', TestConnection('host', 8080), 42);
        final hv = hostValue(nested);
        expect(
          extractHostValue<(String, TestConnection, int)>(hv),
          equals(nested),
        );
      });
    });

    group('Integration with IR system', () {
      test('host values integrate seamlessly with IR', () {
        final w = TestWidget('complex', true);
        final hv = hostValue(w);
        final ir = IrNativeValue(hv);
        expect(isHostValue(ir), isTrue);
        final extracted = getHostValueFromIR(ir);
        expect(extracted, isNotNull);
        expect(extractHostValue<TestWidget>(extracted!), equals(w));
      });

      test('host values work in complex IR structures', () {
        final hv = hostValue(TestWidget('complex', true));
        final listIr = IrList([IrNativeValue(hv), IrString('test')]);
        // The list should contain the host value and string
        expect(listIr.elements.length, equals(2));
        expect(listIr.elements[0], isA<IrNativeValue>());
        expect(listIr.elements[1], equals(IrString('test')));
      });
    });

    group('HostValue evaluation behavior', () {
      test('host values evaluate to themselves (no change)', () async {
        final hv = hostValue(TestWidget('eval', true));
        final ir = IrNativeValue(hv);
        final env = emptyEnv();
        final result = await runEvalSimple(eval(ir), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (evaluated, _) = value;
          // Check that it's still a host value
          expect(isHostValue(evaluated), isTrue);
          // Extract and compare the contents with explicit types
          final origHv = getHostValueFromIR(ir);
          final evaluatedHv = getHostValueFromIR(evaluated);
          expect(origHv, isNotNull);
          expect(evaluatedHv, isNotNull);
          final origWidget = extractHostValue<TestWidget>(origHv!);
          final evaluatedWidget = extractHostValue<TestWidget>(evaluatedHv!);
          expect(origWidget, equals(evaluatedWidget));
        });
      });

      test('host values cannot be called directly (not callable)', () async {
        final hv = hostValue(TestWidget('call', false));
        final hostIr = IrNativeValue(hv);
        final callIr = IrList([hostIr, IrString('arg')]);
        final env = emptyEnv();
        final result = await runEvalSimple(eval(callIr), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (resultIr, _) = value;
          // Should evaluate to a list containing the host value and string
          expect(resultIr, isA<IrList>());
          final listIr = resultIr as IrList;
          expect(listIr.elements.length, equals(2));
          expect(listIr.elements[0], isA<IrNativeValue>());
          expect(listIr.elements[1], equals(IrString('arg')));
        });
      });

      test('host values can be passed as arguments to functions', () async {
        // Create a function that accepts a host value and returns it
        final identityFunc = IrNativeFunc(
          (List<Ir> args) => Eval.pure(args[0]),
        );
        final hv = hostValue(TestWidget('arg', true));
        final hostIr = IrNativeValue(hv);
        final callIr = IrList([identityFunc, hostIr]);
        final env = emptyEnv();
        final result = await runEvalSimple(eval(callIr), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (resultIr, _) = value;
          // Check that result is a host value with the same content
          expect(isHostValue(resultIr), isTrue);
          final origHv = getHostValueFromIR(hostIr);
          final resHv = getHostValueFromIR(resultIr);
          expect(origHv, isNotNull);
          expect(resHv, isNotNull);
          final origWidget = extractHostValue<TestWidget>(origHv!);
          final resWidget = extractHostValue<TestWidget>(resHv!);
          expect(origWidget, equals(resWidget));
        });
      });

      test('functions can return host values', () async {
        // Create a function that returns a host value
        final widget = TestWidget('return', false);
        final hv = hostValue(widget);
        final returnFunc = IrNativeFunc(
          (List<Ir> args) => Eval.pure(IrNativeValue(hv)),
        );
        final callIr = IrList([returnFunc]);
        final env = emptyEnv();
        final result = await runEvalSimple(eval(callIr), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (resultIr, _) = value;
          // Check that result is a host value with the expected content
          expect(isHostValue(resultIr), isTrue);
          final extracted = getHostValueFromIR(resultIr);
          expect(extracted, isNotNull);
          expect(extractHostValue<TestWidget>(extracted!), equals(widget));
        });
      });

      test('host values work in nested function calls', () async {
        // Test: (identity (create-widget "nested"))
        final createWidgetFunc = IrNativeFunc(
          (List<Ir> args) =>
              Eval.pure(IrNativeValue(hostValue(TestWidget('nested', true)))),
        );
        final identityFunc = IrNativeFunc(
          (List<Ir> args) => Eval.pure(args[0]),
        );
        final createCall = IrList([createWidgetFunc]);
        final nestedCall = IrList([identityFunc, createCall]);
        final env = emptyEnv();
        final result = await runEvalSimple(eval(nestedCall), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (resultIr, _) = value;
          expect(isHostValue(resultIr), isTrue);
          final extracted = getHostValueFromIR(resultIr);
          expect(extracted, isNotNull);
          expect(
            extractHostValue<TestWidget>(extracted!),
            equals(TestWidget('nested', true)),
          );
        });
      });

      test('host values in environment work correctly', () async {
        final hv = hostValue(TestWidget('env', false));
        final hostIr = IrNativeValue(hv);
        final env = defineVar('myWidget', hostIr, emptyEnv());
        final symbolIr = IrSymbol('myWidget');
        final result = await runEvalSimple(eval(symbolIr), env);
        expect(result.isRight, isTrue);
        result.match((error) => fail('Should not be left: $error'), (value) {
          final (resultIr, _) = value;
          // Check that result is a host value with the same content
          expect(isHostValue(resultIr), isTrue);
          final origHv = getHostValueFromIR(hostIr);
          final resHv = getHostValueFromIR(resultIr);
          expect(origHv, isNotNull);
          expect(resHv, isNotNull);
          final origWidget = extractHostValue<TestWidget>(origHv!);
          final resWidget = extractHostValue<TestWidget>(resHv!);
          expect(origWidget, equals(resWidget));
        });
      });
    });
  });
}
