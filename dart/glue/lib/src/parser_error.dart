/// Parser error types for the Glue language
/// Mirrors the Haskell Parser/Error.hs implementation
sealed class ParserError {
  const ParserError();

  @override
  String toString();
}

/// Error when properties are mixed with positional arguments
class MixedContentError extends ParserError {
  final String content;

  const MixedContentError(this.content);

  @override
  String toString() =>
      "Syntax Error: Property '$content' cannot be mixed with positional arguments.\n"
      "In Glue LISP, a list must be EITHER all properties (:key val) OR all atoms.";
}

/// Error when a property key has no corresponding value
class UnpairedPropertyError extends ParserError {
  final String key;

  const UnpairedPropertyError(this.key);

  @override
  String toString() => "Syntax Error: The property '$key' is missing a value.";
}

/// General syntax error
class SyntaxError extends ParserError {
  final String message;

  const SyntaxError(this.message);

  @override
  String toString() => "Syntax Error: '$message'";
}
