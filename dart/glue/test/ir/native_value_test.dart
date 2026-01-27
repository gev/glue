import 'package:glue/src/ir.dart';
import 'package:test/test.dart';

// Test data types for host values
class TestWidget {
  final String label;
  final bool enabled;

  const TestWidget(this.label, this.enabled);

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
  bool operator ==(Object other) =>
      other is TestConnection && other.host == host && other.port == port;

  @override
  int get hashCode => Object.hash(host, port);
}

void main() {
  group('Value system for native object integration', () {
    group('Value creation and extraction', () {
      test('creates host value from any typeable value', () {
        final widget = TestWidget('button', true);
        final hv = hostValue(widget);
        expect(hv, isNotNull); // Just check it creates successfully
      });

      test('extracts host value with correct type', () {
        final widget = TestWidget('submit', false);
        final hv = hostValue(widget);
        expect(extractValue<TestWidget>(hv), equals(widget));
      });

      test('fails to extract with wrong type', () {
        final widget = TestWidget('test', true);
        final hv = hostValue(widget);
        expect(extractValue<String>(hv), isNull);
      });

      test('handles different host value types', () {
        final conn = TestConnection('localhost', 5432);
        final hv = hostValue(conn);
        expect(extractValue<TestConnection>(hv), equals(conn));
      });

      test('round-trip: any typeable value can be stored and retrieved', () {
        final widget = TestWidget('test', false);
        final hv = hostValue(widget);
        expect(extractValue<TestWidget>(hv), equals(widget));
      });
    });

    group('Value equality', () {
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

    group('Value in IR system', () {
      test('creates IR with host value', () {
        final widget = TestWidget('test', true);
        final hv = hostValue(widget);
        final ir = IrNativeValue(hv);
        expect(ir, isNotNull);
      });

      test('identifies host value IR', () {
        final hv = hostValue(TestWidget('test', true));
        final ir = IrNativeValue(hv);
        expect(isValue(ir), isTrue);
      });

      test('non-host IR is not identified as host value', () {
        final ir = IrInteger(42);
        expect(isValue(ir), isFalse);
      });

      test('extracts host value from IR', () {
        final widget = TestWidget('extract', false);
        final hv = hostValue(widget);
        final ir = IrNativeValue(hv);
        final extracted = getValueFromIR(ir);
        expect(extracted, isNotNull);
        expect(extractValue<TestWidget>(extracted!), equals(widget));
      });

      test('returns null for non-host IR', () {
        final ir = IrString('not host');
        expect(getValueFromIR(ir), isNull);
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
        expect(extractValue<String>(hv), isNull);
      });

      test('type safety: extraction succeeds only for correct types', () {
        final widget = TestWidget('test', true);
        final hv = hostValue(widget);
        final widgetResult = extractValue<TestWidget>(hv);
        final stringResult = extractValue<String>(hv);
        expect(widgetResult, equals(widget));
        expect(stringResult, isNull);
      });
    });

    group('Host value lifecycle', () {
      test('host values can be created from complex types', () {
        final complex = [TestWidget('a', true), TestWidget('b', false)];
        final hv = hostValue(complex);
        expect(extractValue<List<TestWidget>>(hv), equals(complex));
      });

      test('host values preserve nested structures', () {
        final nested = ('tuple', TestConnection('host', 8080), 42);
        final hv = hostValue(nested);
        expect(extractValue<(String, TestConnection, int)>(hv), equals(nested));
      });
    });
  });
}
