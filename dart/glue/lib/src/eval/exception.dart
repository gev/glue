import 'package:glue/src/ir.dart';

/// Runtime exceptions for Glue evaluation
/// Mirrors Haskell Glue.Eval.Exception exactly
class RuntimeException {
  final String symbol;
  final Ir? value;

  const RuntimeException(this.symbol, this.value);

  String pretty() => 'Runtime Exception: $symbol. ${value ?? 'No details'}';

  @override
  String toString() => pretty();

  @override
  bool operator ==(Object other) =>
      other is RuntimeException &&
      other.symbol == symbol &&
      other.value == value;

  @override
  int get hashCode => Object.hash(symbol, value);
}

/// Factory functions for specific runtime exceptions
/// Mirrors Haskell constructor functions exactly

RuntimeException unboundVariable(String name) =>
    RuntimeException('unbound-variable', IrString(name));

RuntimeException canNotSetUnboundVariable(String name) =>
    RuntimeException('cannot-set-unbound-variable', IrString(name));

RuntimeException notCallableObject() =>
    RuntimeException('not-callable-object', null);

RuntimeException expectedValue() => RuntimeException('expected-value', null);

RuntimeException expectedListOfSymbols() =>
    RuntimeException('expected-list-of-symbols', null);

RuntimeException wrongNumberOfArguments() =>
    RuntimeException('wrong-number-of-arguments', null);

RuntimeException wrongArgumentType(List<String> expected) => RuntimeException(
  'wrong-argument-type',
  IrList(expected.map(IrString.new).toList()),
);

RuntimeException divByZero() => RuntimeException('div-by-zero', null);

RuntimeException propertyNotFound(String property) =>
    RuntimeException('property-not-found', IrString(property));

RuntimeException notAnObject(Ir ir) => RuntimeException('not-an-object', ir);

RuntimeException moduleNotFound(String module) =>
    RuntimeException('module-not-found', IrString(module));

RuntimeException runtimeException(String symbol, Ir value) =>
    RuntimeException(symbol, value);
