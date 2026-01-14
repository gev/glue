/// Either type for handling success/failure results
/// Mirrors Haskell's Either type for functional error handling
/// Implemented as a sealed class for pattern matching
library;

/// Either sealed class - defines the contract for sum types
sealed class Either<L, R> {
  const Either._();

  /// True if this is a Left value
  bool get isLeft => this is Left<L, R>;

  /// True if this is a Right value
  bool get isRight => this is Right<L, R>;
}

/// Represents a result (Left in Haskell Either)
final class Left<L, R> extends Either<L, R> {
  final L value;

  const Left(this.value) : super._();

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
final class Right<L, R> extends Either<L, R> {
  final R value;

  const Right(this.value) : super._();

  @override
  String toString() => 'Right($value)';

  @override
  bool operator ==(Object other) {
    return other is Right<L, R> && value == other.value;
  }

  @override
  int get hashCode => value.hashCode;
}
