/// Either type for handling success/failure results
/// Mirrors Haskell's Either type for functional error handling
/// Implemented as an interface with Left/Right implementations
library;

/// Either interface - defines the contract for sum types
abstract interface class Either<L, R> {
  /// True if this is a Left value (success/result)
  bool get isLeft;

  /// True if this is a Right value (error/failure)
  bool get isRight;

  /// Folds this Either into a single value using the provided functions
  T fold<T>(T Function(L left) onLeft, T Function(R right) onRight);

  /// Maps the left value using the provided function
  Either<L2, R> mapLeft<L2>(L2 Function(L) f);

  /// Maps the right value using the provided function
  Either<L, R2> mapRight<R2>(R2 Function(R) f);
}

/// Represents a successful result (Left in Haskell Either)
final class Left<L, R> implements Either<L, R> {
  final L value;

  const Left(this.value);

  @override
  bool get isLeft => true;

  @override
  bool get isRight => false;

  @override
  T fold<T>(T Function(L left) onLeft, T Function(R right) onRight) {
    return onLeft(value);
  }

  @override
  Either<L2, R> mapLeft<L2>(L2 Function(L) f) {
    return Left<L2, R>(f(value));
  }

  @override
  Either<L, R2> mapRight<R2>(R2 Function(R) f) {
    return Left<L, R2>(value);
  }

  @override
  String toString() => 'Left($value)';

  @override
  bool operator ==(Object other) {
    return other is Left<L, R> && value == other.value;
  }

  @override
  int get hashCode => value.hashCode;
}

/// Represents an error/failure result (Right in Haskell Either)
final class Right<L, R> implements Either<L, R> {
  final R value;

  const Right(this.value);

  @override
  bool get isLeft => false;

  @override
  bool get isRight => true;

  @override
  T fold<T>(T Function(L left) onLeft, T Function(R right) onRight) {
    return onRight(value);
  }

  @override
  Either<L2, R> mapLeft<L2>(L2 Function(L) f) {
    return Right<L2, R>(value);
  }

  @override
  Either<L, R2> mapRight<R2>(R2 Function(R) f) {
    return Right<L, R2>(f(value));
  }

  @override
  String toString() => 'Right($value)';

  @override
  bool operator ==(Object other) {
    return other is Right<L, R> && value == other.value;
  }

  @override
  int get hashCode => value.hashCode;
}
