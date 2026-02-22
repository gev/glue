/// Error types for Glue parser
/// Matches Haskell ParserError types
library;

import 'package:glue/src/error.dart';

class ParseError implements GlueError {
  final String message;

  ParseError(this.message);

  @override
  String pretty() => message;

  @override
  String toString() => message;
}

class SyntaxError extends ParseError {
  SyntaxError(String message) : super('Syntax Error: $message');
}

class MixedContentError extends ParseError {
  MixedContentError(String element)
    : super(
        'Property \'$element\' cannot be mixed with positional arguments.\n'
        'In Glue LISP, a list must be EITHER all properties (:key val) OR all atoms.',
      );
}

class UnpairedPropertyError extends ParseError {
  UnpairedPropertyError(String property)
    : super('The property \'$property\' is missing a value.');
}
