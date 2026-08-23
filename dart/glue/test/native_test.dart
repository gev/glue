import 'package:glue/compile.dart';
import 'package:glue/env.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/parse.dart';
import 'package:test/test.dart';

// Test data types for host objects with mutable state
class Person {
  String name;
  int age;
  Value? addressValue;

  Person(this.name, this.age, this.addressValue);

  @override
  String toString() => 'Person($name, $age, $addressValue)';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is Person &&
          other.name == name &&
          other.age == age &&
          other.addressValue == addressValue);

  @override
  int get hashCode => Object.hash(name, age, addressValue);
}

class Address {
  String street;
  String city;

  Address(this.street, this.city);

  @override
  String toString() => 'Address($street, $city)';

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      (other is Address && other.street == street && other.city == city);

  @override
  int get hashCode => Object.hash(street, city);
}

// Constructor functions that take object literals and create native objects
Eval<Ir> person(Ir arg) {
  return switch (arg) {
    IrObject(properties: final props) => _createPerson(props.unlock),
    _ => throwError(
      RuntimeException('wrong-argument-type', IrString('object')),
    ),
  };
}

Eval<Ir> address(Ir arg) {
  return switch (arg) {
    IrObject(properties: final props) => _createAddress(props.unlock),
    _ => throwError(
      RuntimeException('wrong-argument-type', IrString('object')),
    ),
  };
}

Eval<Ir> _createPerson(Map<String, Ir> props) {
  // Extract properties from object literal with type checking
  final nameIr = props['name'];
  if (nameIr is! IrString) {
    return throwError(
      RuntimeException('wrong-argument-type', IrString('name: string')),
    );
  }
  final name = nameIr.value;

  final ageIr = props['age'];
  if (ageIr is! IrInteger) {
    return throwError(
      RuntimeException('wrong-argument-type', IrString('age: integer')),
    );
  }
  final age = ageIr.value;

  final addressIr = props['address'];
  final Value? addressValue;
  if (addressIr == null) {
    addressValue = null;
  } else if (addressIr is IrNativeValue) {
    final extracted = extractValue<Address>(addressIr.value);
    if (extracted != null) {
      addressValue = addressIr.value;
    } else {
      return throwError(
        RuntimeException('wrong-argument-type', IrString('address: Address')),
      );
    }
  } else {
    return throwError(
      RuntimeException('wrong-argument-type', IrString('address: Address')),
    );
  }

  final personObj = Person(name, age, addressValue);

  final getters = <String, Eval<Ir>>{
    'name': Eval((runtime) => Right((IrString(personObj.name), runtime))),
    'age': Eval((runtime) => Right((IrInteger(personObj.age), runtime))),
    'address': Eval(
      (runtime) => switch (personObj.addressValue) {
        final addrValue? => Right((IrNativeValue(addrValue), runtime)),
        null => Right((IrString('no address'), runtime)),
      },
    ),
  };

  return Eval.pure(IrNativeValue(hostValueWithProps(personObj, getters)));
}

Eval<Ir> _createAddress(Map<String, Ir> props) {
  // Extract properties from object literal with type checking
  final streetIr = props['street'];
  if (streetIr is! IrString) {
    return throwError(
      RuntimeException('wrong-argument-type', IrString('street: string')),
    );
  }
  final street = streetIr.value;

  final cityIr = props['city'];
  if (cityIr is! IrString) {
    return throwError(
      RuntimeException('wrong-argument-type', IrString('city: string')),
    );
  }
  final city = cityIr.value;

  final addrObj = Address(street, city);

  final getters = <String, Eval<Ir>>{
    'street': Eval((runtime) => Right((IrString(addrObj.street), runtime))),
    'city': Eval((runtime) => Right((IrString(addrObj.city), runtime))),
  };

  return Eval.pure(IrNativeValue(hostValueWithProps(addrObj, getters)));
}

// Test environment with constructors
Env testEnv() {
  return defineVar(
    'person',
    IrNativeFunc(person),
    defineVar(
      'address',
      IrNativeFunc(address),
      defineVar(
        'def',
        IrSpecial((List<Ir> args) {
          if (args.length != 2) {
            return throwError(
              RuntimeException('wrong-number-of-arguments', IrString('2')),
            );
          }
          final String name;
          if (args[0] case IrSymbol(value: final n)) {
            name = n;
          } else {
            return throwError(
              RuntimeException('wrong-argument-type', IrString('symbol')),
            );
          }
          return eval(
            args[1],
          ).bind((value) => defineVarEval(name, value).map((_) => value));
        }),
        emptyEnv(),
      ),
    ),
  );
}

// Helper to run Glue code
Either<EvalError, Ir> runGlueCode(String input) {
  final parseResult = parseGlue(input);
  return parseResult.match(
    (parseError) => Left(
      EvalError(
        [],
        RuntimeException('parse-error', IrString(parseError.message)),
      ),
    ),
    (ast) {
      final ir = compile(ast);
      final result = runEvalSimple(eval(ir), testEnv());
      return result.match((error) => Left(error), (value) {
        final (res, _) = value;
        // Handle implicit sequence semantics like Haskell evalBody
        return switch (res) {
          IrList(:final elements) => Right(
            elements.isEmpty ? IrVoid() : elements.last,
          ),
          _ => Right(res),
        };
      });
    },
  );
}

void main() {
  group('Full FFI Integration Tests', () {
    group('Basic Object Creation and Property Access', () {
      test('creates person and accesses properties', () {
        final result = runGlueCode('''
          ((def bob (person :name "Bob" :age 25))
           bob.name)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Bob')));
        });
      });

      test('creates address and accesses properties', () {
        final result = runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           addr.street)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('123 Main St')));
        });
      });
    });

    group('Complex Object Relationships', () {
      test('creates person with address', () {
        final result = runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (def bob (person :name "Bob" :age 25 :address addr))
           bob.address.city)
          ''');
        result.match((error) => fail('Error: $error'), (value) {
          expect(value, equals(IrString('Springfield')));
        });
      });
    });

    group('Error Handling', () {
      test('fails with wrong constructor arguments', () {
        final result = runGlueCode('(person "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong name type', () {
        final result = runGlueCode('(person :name 123 :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong age type', () {
        final result = runGlueCode('(person :name "Bob" :age "25")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong address type', () {
        final result = runGlueCode(
          '(person :name "Bob" :age 25 :address "not-an-address")',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with missing name field', () {
        final result = runGlueCode('(person :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing age field', () {
        final result = runGlueCode('(person :name "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong street type', () {
        final result = runGlueCode('(address :street 123 :city "Springfield")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong city type', () {
        final result = runGlueCode('(address :street "123 Main St" :city 456)');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing street field', () {
        final result = runGlueCode('(address :city "Springfield")');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing city field', () {
        final result = runGlueCode('(address :street "123 Main St")');
        expect(result.isLeft, isTrue);
      });

      test('returns Void when accessing non-existent properties', () {
        final result = runGlueCode(
          '((def bob (person :name "Bob" :age 25)) bob.nonexistent)',
        );
        result.match((error) => fail('Error: $error'), (value) {
          expect(value, equals(IrVoid()));
        });
      });
    });
  });
}
