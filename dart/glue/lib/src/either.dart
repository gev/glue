/// Either type for handling success/failure results
/// Mirrors Haskell's Either type for functional error handling
/// Implemented as a sealed class for pattern matching
library;

/// Either interface - defines the contract for sum types
abstract interface class Either<L, R> {
  /// Pattern matching method - provides functional API for handling both cases
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight);

  /// Map over the Right value
  Either<L, T> map<T>(T Function(R r) f);

  /// True if this is a Left value
  bool get isLeft;

  /// True if this is a Right value
  bool get isRight;
}

/// Represents a result (Left in Haskell Either)
final class Left<L, R> implements Either<L, R> {
  final L value;

  const Left(this.value);

  @override
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight) =>
      onLeft(value);

  @override
  Either<L, T> map<T>(T Function(R r) f) => Left<L, T>(value);

  @override
  bool get isLeft => true;

  @override
  bool get isRight => false;

  @override
  String toString() => 'Left($value)';

  @override
  bool operator ==(Object other) {
    return other is Left<L, R> && value == other.value;
  }

  @override
  int get hashCode => value.hashCode;
}

/// Represents a result (Right in Haskell Either)
final class Right<L, R> implements Either<L, R> {
  final R value;

  const Right(this.value);

  @override
  T match<T>(T Function(L left) onLeft, T Function(R right) onRight) =>
      onRight(value);

  @override
  Either<L, T> map<T>(T Function(R r) f) => Right<L, T>(f(value));

  @override
  bool get isLeft => false;

  @override
  bool get isRight => true;

  @override
  String toString() => 'Right($value)';

  @override
  bool operator ==(Object other) {
    return other is Right<L, R> && value == other.value;
  }

  @override
  int get hashCode => value.hashCode;
}
