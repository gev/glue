/// Error types for Glue parser
/// Matches Haskell ParserError types
library;

import 'package:glue/src/error.dart';

class ParserError implements GlueError {
  final String message;

  ParserError(this.message);

  @override
  String pretty() => message;

  @override
  String toString() => message;
}

class SyntaxError extends ParserError {
  SyntaxError(String message) : super('Syntax Error: $message');
}

class MixedContentError extends ParserError {
  MixedContentError(String element)
    : super(
        'Property \'$element\' cannot be mixed with positional arguments.\n'
        'In Glue LISP, a list must be EITHER all properties (:key val) OR all atoms.',
      );
}

class UnpairedPropertyError extends ParserError {
  UnpairedPropertyError(String property)
    : super('The property \'$property\' is missing a value.');
}
