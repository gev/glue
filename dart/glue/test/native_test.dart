import 'package:glue/env.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue/src/ast.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/lib/builtin/set.dart';
import 'package:glue/src/parser.dart';
import 'package:glue/src/runtime.dart';
import 'package:test/test.dart';

// Test data types for host objects with mutable state
class Person {
  String name;
  int age;
  HostValue? addressHostValue;

  Person(this.name, this.age, this.addressHostValue);

  @override
  String toString() => 'Person($name, $age, $addressHostValue)';

  @override
  bool operator ==(Object other) =>
      other is Person &&
      other.name == name &&
      other.age == age &&
      other.addressHostValue == addressHostValue;

  @override
  int get hashCode => Object.hash(name, age, addressHostValue);
}

class Address {
  String street;
  String city;

  Address(this.street, this.city);

  @override
  String toString() => 'Address($street, $city)';

  @override
  bool operator ==(Object other) =>
      other is Address && other.street == street && other.city == city;

  @override
  int get hashCode => Object.hash(street, city);
}

// Constructor functions that take object literals and create native objects
Eval<Ir> person(List<Ir> args) {
  return switch (args) {
    [IrObject(properties: final props)] => _createPerson(props.unlock),
    _ => throwError(
      RuntimeException('wrong-argument-type', IrString('object')),
    ),
  };
}

Eval<Ir> address(List<Ir> args) {
  return switch (args) {
    [IrObject(properties: final props)] => _createAddress(props.unlock),
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
  final HostValue? addressHostValue;
  if (addressIr == null) {
    addressHostValue = null;
  } else if (addressIr is IrNativeValue) {
    final extracted = extractHostValue<Address>(addressIr.value);
    if (extracted != null) {
      addressHostValue = addressIr.value;
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

  final personObj = Person(name, age, addressHostValue);

  // Create getters and setters
  final getters = <String, Eval<Ir>>{
    'name': Eval((runtime) => Right((IrString(personObj.name), runtime))),
    'age': Eval((runtime) => Right((IrInteger(personObj.age), runtime))),
    'address': Eval(
      (runtime) => switch (personObj.addressHostValue) {
        final addrHostValue? => Right((IrNativeValue(addrHostValue), runtime)),
        null => Right((IrString('no address'), runtime)),
      },
    ),
  };

  final setters = <String, Eval<Ir> Function(Ir)>{
    'name': (Ir value) => switch (value) {
      IrString(value: final newName) => Eval<Ir>((runtime) {
        personObj.name = newName;
        return Right((IrVoid(), runtime));
      }),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
    'age': (Ir value) => switch (value) {
      IrInteger(value: final newAge) => Eval<Ir>((runtime) {
        personObj.age = newAge;
        return Right((IrVoid(), runtime));
      }),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('integer')),
      ),
    },
    'address': (Ir value) => switch (value) {
      IrNativeValue(value: final addrHostValue) =>
        switch (extractHostValue<Address>(addrHostValue)) {
          final addr? => Eval<Ir>((runtime) {
            personObj.addressHostValue = addrHostValue;
            return Right((IrVoid(), runtime));
          }),
          null => throwError(
            RuntimeException(
              'wrong-argument-type',
              IrString('address: Address'),
            ),
          ),
        },
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('NativeValue')),
      ),
    },
  };

  return Eval.pure(
    IrNativeValue(hostValueWithProps(personObj, getters, setters)),
  );
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

  // Create getters and setters
  final getters = <String, Eval<Ir>>{
    'street': Eval((runtime) => Right((IrString(addrObj.street), runtime))),
    'city': Eval((runtime) => Right((IrString(addrObj.city), runtime))),
  };

  final setters = <String, Eval<Ir> Function(Ir)>{
    'street': (Ir value) => switch (value) {
      IrString(value: final newStreet) => Eval<Ir>((runtime) {
        addrObj.street = newStreet;
        return Right((IrVoid(), runtime));
      }),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
    'city': (Ir value) => switch (value) {
      IrString(value: final newCity) => Eval<Ir>((runtime) {
        addrObj.city = newCity;
        return Right((IrVoid(), runtime));
      }),
      _ => throwError(
        RuntimeException('wrong-argument-type', IrString('string')),
      ),
    },
  };

  return Eval.pure(
    IrNativeValue(hostValueWithProps(addrObj, getters, setters)),
  );
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
          final name;
          if (args[0] case IrSymbol(value: final n)) {
            name = n;
          } else {
            return throwError(
              RuntimeException('wrong-argument-type', IrString('symbol')),
            );
          }
          return eval(
            args[1],
          ).flatMap((value) => defineVarEval(name, value).map((_) => value));
        }),
        defineVar('set', IrSpecial(set), emptyEnv()),
      ),
    ),
  );
}

// Helper to run Glue code
Future<Either<EvalError, Ir>> runGlueCode(String input) async {
  final parseResult = parseGlue(input);
  return parseResult.match(
    (parseError) => Left(
      EvalError(
        [],
        RuntimeException('parse-error', IrString(parseError.message)),
      ),
    ),
    (ast) async {
      final ir = compile(ast);
      final result = await runEvalSimple(eval(ir), testEnv());
      return result.match((error) => Left<EvalError, Ir>(error), (value) {
        final (res, _) = value;
        // Handle implicit sequence semantics like Haskell evalBody
        return switch (res) {
          IrList(elements: []) => Right<EvalError, Ir>(IrVoid()),
          IrList(elements: final elements) when elements.isNotEmpty =>
            Right<EvalError, Ir>(elements.last),
          _ => Right<EvalError, Ir>(res),
        };
      });
    },
  );
}

void main() {
  group('Full FFI Integration Tests', () {
    group('Basic Object Creation and Property Access', () {
      test('creates person and accesses properties', () async {
        final result = await runGlueCode('''
          ((def bob (person :name "Bob" :age 25))
           bob.name)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Bob')));
        });
      });

      test('creates address and accesses properties', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           addr.street)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('123 Main St')));
        });
      });
    });

    group('Property Modification', () {
      test('modifies person properties', () async {
        final result = await runGlueCode('''
          ((def bob (person :name "Bob" :age 25))
           (set bob.age 26)
           bob.age)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrInteger(26)));
        });
      });

      test('modifies address properties', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (set addr.city "Boston")
           addr.city)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Complex Object Relationships', () {
      test('creates person with address', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (def bob (person :name "Bob" :age 25 :address addr))
           bob.address.city)
          ''');
        result.match((error) => fail('Error: $error'), (value) {
          expect(value, equals(IrString('Springfield')));
        });
      });

      test('modifies nested properties', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (def bob (person :name "Bob" :age 25 :address addr))
           (set bob.address.city "Boston")
           bob.address.city)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Multiple Operations in Sequence', () {
      test('performs complex object manipulation', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (def bob (person :name "Bob" :age 25 :address addr))
           (set bob.age 26)
           (set bob.name "Robert")
           (set bob.address.city "Boston")
           (set bob.address.street "456 Oak Ave")
           bob.name)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Robert')));
        });
      });

      test('verifies all modifications persist', () async {
        final result = await runGlueCode('''
          ((def addr (address :street "123 Main St" :city "Springfield"))
           (def bob (person :name "Bob" :age 25 :address addr))
           (set bob.age 26)
           (set bob.address.city "Boston")
           bob.age)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrInteger(26)));
        });
      });

      test('sets new address on person', () async {
        final result = await runGlueCode('''
          ((def addr1 (address :street "123 Main St" :city "Springfield"))
           (def addr2 (address :street "456 Oak Ave" :city "Boston"))
           (def bob (person :name "Bob" :age 25 :address addr1))
           (set bob.address addr2)
           bob.address.city)
          ''');
        result.match((error) => fail('Should not be left: $error'), (value) {
          expect(value, equals(IrString('Boston')));
        });
      });
    });

    group('Error Handling', () {
      test('fails with wrong constructor arguments', () async {
        final result = await runGlueCode('(person "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong name type', () async {
        final result = await runGlueCode('(person :name 123 :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong age type', () async {
        final result = await runGlueCode('(person :name "Bob" :age "25")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong address type', () async {
        final result = await runGlueCode(
          '(person :name "Bob" :age 25 :address "not-an-address")',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with missing name field', () async {
        final result = await runGlueCode('(person :age 25)');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing age field', () async {
        final result = await runGlueCode('(person :name "Bob")');
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong street type', () async {
        final result = await runGlueCode(
          '(address :street 123 :city "Springfield")',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with wrong city type', () async {
        final result = await runGlueCode(
          '(address :street "123 Main St" :city 456)',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails with missing street field', () async {
        final result = await runGlueCode('(address :city "Springfield")');
        expect(result.isLeft, isTrue);
      });

      test('fails with missing city field', () async {
        final result = await runGlueCode('(address :street "123 Main St")');
        expect(result.isLeft, isTrue);
      });

      test('fails accessing non-existent properties', () async {
        final result = await runGlueCode(
          '((def bob (person :name "Bob" :age 25)) bob.nonexistent)',
        );
        expect(result.isLeft, isTrue);
      });

      test('fails setting wrong types', () async {
        final result = await runGlueCode(
          '((def bob (person :name "Bob" :age 25)) (set bob.age "not-a-number"))',
        );
        expect(result.isLeft, isTrue);
      });
    });
  });
}
