/// Either type for handling success/failure results
/// Mirrors Haskell's Either type for functional error handling
/// Implemented as a sealed class for pattern matching
library;

/// Either interface - defines the contract for sum types
abstract interface class Either<L, R> {
  /// Pattern matching method - provides functional API for handling both cases
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight);

  /// True if this is a Left value
  bool get isLeft;

  /// True if this is a Right value
  bool get isRight;
}

/// Represents a result (Left in Haskell Either)
final class Left<L, R> implements Either<L, R> {
  final L _value;

  const Left(this._value);

  @override
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight) =>
      onLeft(_value);

  @override
  bool get isLeft => true;

  @override
  bool get isRight => false;

  @override
  String toString() => 'Left($_value)';

  @override
  bool operator ==(Object other) {
    return other is Left<L, R> && _value == other._value;
  }

  @override
  int get hashCode => _value.hashCode;
}

/// Represents a result (Right in Haskell Either)
final class Right<L, R> implements Either<L, R> {
  final R _value;

  const Right(this._value);

  @override
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight) =>
      onRight(_value);

  @override
  bool get isLeft => false;

  @override
  bool get isRight => true;

  @override
  String toString() => 'Right($_value)';

  @override
  bool operator ==(Object other) {
    return other is Right<L, R> && _value == other._value;
  }

  @override
  int get hashCode => _value.hashCode;
}
