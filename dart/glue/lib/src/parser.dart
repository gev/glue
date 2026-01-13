import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:petitparser/petitparser.dart';
import 'ast.dart';
import 'either.dart';
import 'parser_errors.dart';

/// Integer parser - matches Haskell pInteger
Parser<Ast> pInteger() {
  return (whitespace().star() &
          (char('-').optional() & digit().plus()).flatten() &
          whitespace().star() &
          char('.').not())
      .map((result) {
        final str = result[1] as String;
        final value = int.parse(str);
        return IntegerAst(value);
      });
}

/// Float parser - matches Haskell pFloat
Parser<Ast> pFloat() {
  // Match valid float patterns: decimal or scientific notation
  final floatPattern =
      (char('-').optional() &
              digit().plus() &
              (char('.') & digit().plus()).optional() &
              ((char('e') | char('E')) &
                      (char('+') | char('-')).optional() &
                      digit().plus())
                  .optional())
          .flatten();

  return (whitespace().star() & floatPattern & whitespace().star()).map((
    result,
  ) {
    final str = result[1] as String;

    // Validate that this is actually a float (contains . or e/E)
    if (!str.contains('.') && !str.contains('e') && !str.contains('E')) {
      throw FormatException('Invalid float format: $str');
    }

    try {
      final value = double.parse(str);
      return FloatAst(value);
    } catch (e) {
      throw FormatException('Invalid float format: $str');
    }
  });
}

/// String parser - matches Haskell pString
Parser<Ast> pString() {
  return (whitespace().star() &
          (char('"') & any().starLazy(char('"')).flatten() & char('"'))
              .flatten() &
          whitespace().star())
      .map((result) {
        final str = result[1] as String;
        final content = str.substring(1, str.length - 1);
        return StringAst(content);
      });
}

/// Symbol parser - matches Haskell pSymbol
Parser<Ast> pSymbol() {
  return (whitespace().star() &
          (letter() | digit() | anyOf("-._:!?\\=<>/*+%\$@#&|'"))
              .plus()
              .flatten() &
          whitespace().star())
      .map((result) => SymbolAst(result[1] as String));
}

/// Expression or List parser - matches Haskell pExprOrList
Parser<Ast> pExprOrList(Parser<Ast> expression) {
  return (whitespace().star() &
          char('(') &
          expression.star() &
          char(')') &
          whitespace().star())
      .map((result) {
        final elements = result[2] as List<Ast>;

        if (elements.isEmpty) {
          // Empty list: ()
          return ListAst(IList([]));
        }

        final first = elements[0];

        // Check if first element is a non-property symbol (function call)
        if (first is SymbolAst && !first.value.startsWith(':')) {
          // Function call: parse rest as body
          final rest = elements.sublist(1);
          return _parseBodyRest(rest, first);
        } else {
          // Object or data list: parse with all elements
          return _parseBodyRest(elements, null);
        }
      });
}

/// Parse the body of a list/object expression
/// This matches Haskell's pBodyRest logic
Ast _parseBodyRest(List<Ast> initial, Ast? functionName) {
  if (initial.isEmpty) {
    return functionName != null
        ? ListAst(IList([functionName]))
        : ListAst(IList([]));
  }

  final first = initial[0];

  // Check if this should be an object (first element is property)
  if (first is SymbolAst && first.value.startsWith(':')) {
    final object = _validateAsObject(
      initial,
      null,
    ); // Don't pass functionName here
    // If we have a function name, wrap the object in a list with the function
    return functionName != null
        ? ListAst(IList([functionName, object]))
        : object;
  } else {
    return _validateAsList(initial, functionName);
  }
}

/// Validate and create an object from property list
Ast _validateAsObject(List<Ast> elements, Ast? functionName) {
  if (functionName != null) {
    // Can't have function name with object syntax
    throw MixedContentError(functionName.toString());
  }

  final properties = <String, Ast>{};
  var i = 0;

  while (i < elements.length) {
    final element = elements[i];

    if (element is SymbolAst && element.value.startsWith(':')) {
      final key = element.value.substring(1); // Remove ':'

      if (i + 1 >= elements.length) {
        throw UnpairedPropertyError(element.value);
      }

      final value = elements[i + 1];
      properties[key] = value;
      i += 2; // Skip key and value
    } else {
      throw MixedContentError(element.toString());
    }
  }

  return ObjectAst(IMap(properties));
}

/// Validate and create a list
Ast _validateAsList(List<Ast> elements, Ast? functionName) {
  // For function calls, property symbols are allowed (they become objects)
  // Only check for mixed content if this is NOT a function call
  if (functionName == null) {
    // Check for any property symbols in the elements (data lists can't have properties)
    for (final element in elements) {
      if (element is SymbolAst && element.value.startsWith(':')) {
        throw MixedContentError(element.toString());
      }
    }
  }

  final allElements = functionName != null
      ? [functionName, ...elements]
      : elements;
  return ListAst(IList(allElements));
}

/// Petitparser-based parser for Glue language
/// Matches Haskell Megaparsec implementation with proper error handling
class GlueParser {
  late final Parser<Ast> parser;

  GlueParser() {
    parser = _buildParser();
  }

  Parser<Ast> _buildParser() {
    // Comments: ; to end of line
    final comment = (string(';') & any().starLazy(char('\n') | endOfInput()))
        .flatten();

    // Whitespace with comments
    final ws = (whitespace() | comment).star();

    // Create recursive parser using petitparser's built-in recursion support
    final expression = undefined<Ast>();

    // Define the recursive parser with all atom parsers and list parser
    // Matches Haskell: choice [ pExprOrList, pString, pInteger, pFloat, pSymbol ]
    expression.set(
      (pExprOrList(expression) | pString() | pInteger() | pFloat() | pSymbol())
          .cast<Ast>(),
    );

    // Main parser - parse one expression
    return (ws & expression & ws).map((result) => result[1] as Ast);
  }

  /// Parse Glue source code
  /// Returns Result<Ast, ParserError> matching Haskell Either
  Either<Ast, ParserError> parse(String input) {
    try {
      final result = parser.parse(input);
      if (result is Success) {
        return Left(result.value);
      } else {
        return Right(SyntaxError((result as Failure).message));
      }
    } catch (e) {
      if (e is ParserError) {
        return Right(e);
      }
      return Right(SyntaxError(e.toString()));
    }
  }
}

/// Convenience function to parse Glue code
/// Returns either Ast on success or ParserError on failure
Object parseGlue(String input) {
  final parser = GlueParser();
  final result = parser.parse(input);
  return result.fold<Object>(
    (ast) => ast, // Success case
    (error) => error, // Error case
  );
}
