import 'package:petitparser/petitparser.dart';
import 'ast.dart';
import 'either.dart';
import 'parser_errors.dart';

/// Integer parser - matches Haskell pInteger
/// SRP: Only handles integer parsing logic, no whitespace
Parser<String> pIntegerToken() {
  return (char('-').optional() & digit().plus() & char('.').not()).flatten(
    message: 'Integer expected',
  );
}

/// Float parser - matches Haskell pFloat
/// SRP: Only handles float parsing logic, no whitespace
Parser<String> pFloatToken() {
  return (char('-').optional() &
          digit().plus() &
          char('.').seq(digit().plus()).optional() &
          anyOf(
            'eE',
          ).seq(anyOf('-+').optional()).seq(digit().plus()).optional())
      .flatten(message: 'Float expected');
}

/// String parser - matches Haskell pString
/// SRP: Only handles string parsing logic, no whitespace
Parser<String> pStringToken() {
  final character = (char('\\') & any()) | pattern('^"');
  return (char('"') & character.star() & char('"')).flatten();
}

/// Symbol parser - matches Haskell pSymbol
/// SRP: Only handles symbol parsing logic, no whitespace
/// Supports Unicode characters like Haskell's alphaNumChar
Parser<String> pSymbolToken() {
  // Allow any character except whitespace and structural delimiters
  // This matches Haskell's approach of allowing Unicode in symbols
  return (noneOf('()[]{} \t\n\r') & noneOf('()[]{} \t\n\r').star()).flatten(
    message: 'Symbol expected',
  );
}

/// Expression or List parser - matches Haskell pExprOrList
/// SRP: Only handles list parsing logic, whitespace handled at higher level
Parser<Ast> pExprOrList(Parser<Ast> expression) {
  // Parse expressions with whitespace between them
  final exprWithWs = expression.trim();
  return (char('(').trim() & exprWithWs.star() & char(')').trim()).map((
    result,
  ) {
    final elements = result[1] as List<Ast>;

    if (elements.isEmpty) {
      // Empty list: ()
      return ListAst([]);
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
    return functionName != null ? ListAst([functionName]) : ListAst([]);
  }

  final first = initial[0];

  // Check if this should be an object (first element is property)
  if (first is SymbolAst && first.value.startsWith(':')) {
    final object = _validateAsObject(
      initial,
      null,
    ); // Don't pass functionName here
    // If we have a function name, wrap the object in a list with the function
    return functionName != null ? ListAst([functionName, object]) : object;
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

  return ObjectAst(properties);
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
  return ListAst(allElements);
}

/// Petitparser-based parser for Glue language
/// Follows SRP: whitespace handling separated from parsing logic
class GlueParser {
  late final Parser<Ast> parser;

  GlueParser() {
    parser = _buildParser();
  }

  Parser<Ast> _buildParser() {
    // Comments: ; to end of line
    final comment = (string(';') & any().starLazy(char('\n') | endOfInput()))
        .flatten();

    // Whitespace with comments (handles whitespace globally)
    final ws = (whitespace() | comment).star();

    // Create recursive parser using petitparser's built-in recursion support
    final expression = undefined<Ast>();

    // Define the recursive expression parser first
    expression.set(_buildExpressionParser(expression));

    // Main parser - parse one expression with global whitespace handling
    return (ws & expression & ws).map((result) => result[1] as Ast);
  }

  Parser<Ast> _buildExpressionParser(Parser<Ast> expression) {
    // AST mapping parsers (SRP: only handle AST construction)
    final integerParser = pIntegerToken().map(
      (value) => IntegerAst(int.parse(value)),
    );
    final floatParser = pFloatToken().map((value) {
      // Validate that this is actually a float (contains . or e/E)
      if (!value.contains('.') &&
          !value.contains('e') &&
          !value.contains('E')) {
        throw FormatException('Invalid float format: $value');
      }
      return FloatAst(double.parse(value));
    });
    final stringParser = pStringToken().map((value) {
      final content = value.substring(1, value.length - 1);
      return StringAst(content);
    });
    final symbolParser = pSymbolToken().map((value) => SymbolAst(value));
    final listParser = pExprOrList(expression);

    // Define the parser with all atom parsers and list parser
    // Whitespace handled at this level (SRP: separation of concerns)
    return (listParser |
            integerParser |
            floatParser |
            stringParser |
            symbolParser)
        .cast<Ast>();
  }

  /// Parse Glue source code
  /// Returns Either<ParserError, Ast> following functional convention: Left=Error, Right=Success
  Either<ParserError, Ast> parse(String input) {
    try {
      final result = parser.parse(input);
      if (result is Success) {
        return Right(result.value); // Success = Right
      } else {
        return Left(SyntaxError((result as Failure).message)); // Error = Left
      }
    } catch (e) {
      if (e is ParserError) {
        return Left(e); // Error = Left
      }
      return Left(SyntaxError(e.toString())); // Error = Left
    }
  }
}

/// Convenience function to parse Glue code
/// Returns Either<ParserError, Ast> for type-safe error handling
/// Follows functional programming convention: Left=Error, Right=Success
Either<ParserError, Ast> parseGlue(String input) {
  final parser = GlueParser();
  return parser.parse(input);
}
