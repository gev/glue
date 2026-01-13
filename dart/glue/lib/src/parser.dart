import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'ast.dart';
import 'parser_error.dart';

/// Result type for parsing operations
class ParseResult {
  final Ast? ast;
  final ParserError? error;

  ParseResult.success(this.ast) : error = null;
  ParseResult.failure(this.error) : ast = null;

  bool get isSuccess => ast != null;
  bool get isFailure => error != null;

  Object get value => ast ?? error!;
}

/// Simple parser for Glue language (basic implementation)
class GlueParser {
  /// Parse a Glue source string into an AST or error
  static Object parse(String input) {
    // Remove comments first
    final cleaned = _removeComments(input.trim());
    if (cleaned.isEmpty) return SyntaxError("Empty input");

    // Try parsing as different AST types
    final result = _parseWithResult(cleaned);
    return result.value;
  }

  static ParseResult _parseWithResult(String input) {
    try {
      final result =
          _parseInteger(input) ??
          _parseFloat(input) ??
          _parseString(input) ??
          _parseSymbol(input) ??
          _parseObject(input) ?? // Try objects before lists
          _parseList(input);

      return result != null
          ? ParseResult.success(result)
          : ParseResult.failure(SyntaxError("Invalid syntax: '$input'"));
    } catch (e) {
      if (e is ParserError) {
        return ParseResult.failure(e);
      }
      return ParseResult.failure(SyntaxError("Unexpected error: $e"));
    }
  }
}

String _removeComments(String input) {
  // Simple comment removal - replace ; comments with nothing
  final lines = input.split('\n');
  final cleaned = lines
      .map((line) {
        final commentIndex = line.indexOf(';');
        return commentIndex >= 0 ? line.substring(0, commentIndex) : line;
      })
      .join('\n')
      .trim();

  return cleaned;
}

Ast? _parseInteger(String input) {
  final trimmed = input.trim();
  final intValue = int.tryParse(trimmed);
  return intValue != null ? IntegerAst(intValue) : null;
}

Ast? _parseFloat(String input) {
  final trimmed = input.trim();
  // Check if it contains decimal point or scientific notation
  if (trimmed.contains('.') || trimmed.contains('e') || trimmed.contains('E')) {
    final doubleValue = double.tryParse(trimmed);
    return doubleValue != null ? FloatAst(doubleValue) : null;
  }
  return null;
}

Ast? _parseString(String input) {
  final trimmed = input.trim();
  if (trimmed.startsWith('"') && trimmed.endsWith('"') && trimmed.length >= 2) {
    final content = trimmed.substring(1, trimmed.length - 1);
    return StringAst(content);
  }
  return null;
}

Ast? _parseSymbol(String input) {
  final trimmed = input.trim();
  // Symbol validation: must not be empty, not start with (, {, ", not be a valid number,
  // and not look like a malformed number
  if (trimmed.isNotEmpty &&
      !trimmed.startsWith('(') &&
      !trimmed.startsWith('{') &&
      !trimmed.startsWith('"') &&
      !_isValidNumber(trimmed) &&
      !_looksLikeMalformedNumber(trimmed)) {
    return SymbolAst(trimmed);
  }
  return null;
}

bool _looksLikeMalformedNumber(String input) {
  // Reject strings that start with a digit and have multiple dots or e/E
  if (!input.startsWith(RegExp(r'[0-9]'))) return false;

  final dotCount = '.'.allMatches(input).length;
  final eCount = 'e'.allMatches(input.toLowerCase()).length;
  return dotCount > 1 || eCount > 1;
}

bool _isValidNumber(String input) {
  // Check if it's a valid integer or float
  return int.tryParse(input) != null || double.tryParse(input) != null;
}

Ast? _parseList(String input) {
  final trimmed = input.trim();
  if (!trimmed.startsWith('(') || !trimmed.endsWith(')')) return null;

  final content = trimmed.substring(1, trimmed.length - 1).trim();
  if (content.isEmpty) return ListAst(IList());

  // Split content into tokens, respecting parentheses and braces
  final tokens = _tokenize(content);
  if (tokens.isEmpty) return ListAst(IList());

  // Parse each token
  final elements = <Ast>[];
  for (final token in tokens) {
    final ast = _parseToken(token);
    if (ast == null) return null; // Parse error
    elements.add(ast);
  }

  // Validate list contents (mixed content rule)
  _validateListContents(elements);

  return ListAst(IList(elements));
}

/// Validates list contents according to Glue rules
/// Throws ParserError if validation fails
void _validateListContents(List<Ast> elements) {
  if (elements.isEmpty) return;

  final hasProperties = elements.any(_isProperty);
  final hasAtoms = elements.any((e) => !_isProperty(e));

  if (hasProperties && hasAtoms) {
    // Find the first atom that's mixed with properties
    final firstAtom = elements.firstWhere((e) => !_isProperty(e));
    throw MixedContentError((firstAtom as dynamic).toString());
  }

  // Check for unpaired property keys
  for (var i = 0; i < elements.length; i++) {
    final element = elements[i];
    if (_isProperty(element) && i == elements.length - 1) {
      // Last element is a property key without value
      throw UnpairedPropertyError((element as dynamic).toString());
    }
  }
}

bool _isProperty(Ast ast) {
  return ast is ObjectAst;
}

/// Tokenizes input string, respecting parentheses and braces
List<String> _tokenize(String input) {
  final tokens = <String>[];
  var current = '';
  var i = 0;

  while (i < input.length) {
    final char = input[i];

    if (char == ' ') {
      if (current.isNotEmpty) {
        tokens.add(current);
        current = '';
      }
    } else if (char == '(' || char == ')' || char == '{' || char == '}') {
      if (current.isNotEmpty) {
        tokens.add(current);
        current = '';
      }
      tokens.add(char);
    } else if (char == '"') {
      // Handle string literals
      if (current.isNotEmpty) {
        tokens.add(current);
        current = '';
      }
      var stringStart = i;
      i++; // Skip opening quote
      while (i < input.length &&
          (input[i] != '"' || (i > 0 && input[i - 1] == '\\'))) {
        i++;
      }
      if (i < input.length) {
        i++; // Skip closing quote
      }
      tokens.add(input.substring(stringStart, i));
      continue;
    } else if (char == ':') {
      // Handle property syntax :key value
      if (current.isNotEmpty) {
        tokens.add(current);
        current = '';
      }
      // Find the end of the property key
      var keyStart = i;
      i++; // Skip :
      while (i < input.length && input[i] != ' ') {
        i++;
      }
      tokens.add(input.substring(keyStart, i));
      continue;
    } else {
      current += char;
    }

    i++;
  }

  if (current.isNotEmpty) {
    tokens.add(current);
  }

  return tokens;
}

/// Parses a single token into an AST
Ast? _parseToken(String token) {
  // Try parsing as different AST types
  return _parseInteger(token) ??
      _parseFloat(token) ??
      _parseString(token) ??
      _parseSymbol(token) ??
      _parseList(token) ??
      _parseObject(token);
}

/// Result of parsing a single element
class _ParseResult {
  final Ast ast;
  final String remaining;

  _ParseResult(this.ast, this.remaining);
}

_ParseResult? _parseNextElement(String input) {
  if (input.isEmpty) return null;

  // Try parsing different types
  final parsers = [
    _parseInteger,
    _parseFloat,
    _parseString,
    _parseSymbol,
    _parseList,
    _parseObject,
  ];

  for (final parser in parsers) {
    final result = parser(input);
    if (result != null) {
      // Find where this element ends
      final endIndex = _findElementEnd(input);
      if (endIndex == -1) return null;

      return _ParseResult(result, input.substring(endIndex));
    }
  }

  return null;
}

int _findElementEnd(String input) {
  var i = 0;
  var parenDepth = 0;
  var braceDepth = 0;
  var inString = false;
  var escaped = false;

  while (i < input.length) {
    final char = input[i];

    if (escaped) {
      escaped = false;
      i++;
      continue;
    }

    if (inString) {
      if (char == '\\') {
        escaped = true;
      } else if (char == '"') {
        inString = false;
      }
      i++;
      continue;
    }

    switch (char) {
      case '"':
        inString = true;
        break;
      case '(':
        parenDepth++;
        break;
      case ')':
        parenDepth--;
        if (parenDepth < 0) return i; // End of element
        break;
      case '{':
        braceDepth++;
        break;
      case '}':
        braceDepth--;
        if (braceDepth < 0) return i; // End of element
        break;
      case ' ':
        if (parenDepth == 0 && braceDepth == 0) {
          return i; // End of element
        }
        break;
    }

    i++;
  }

  return parenDepth == 0 && braceDepth == 0 ? input.length : -1;
}

Ast? _parseObject(String input) {
  final trimmed = input.trim();
  // Objects can be in {} syntax or (:key value) syntax
  final isBraceSyntax = trimmed.startsWith('{') && trimmed.endsWith('}');
  final isColonSyntax =
      trimmed.startsWith('(') &&
      trimmed.endsWith(')') &&
      trimmed.length > 2 &&
      trimmed[1] == ':';

  if (!isBraceSyntax && !isColonSyntax) return null;

  String content;
  if (isBraceSyntax) {
    content = trimmed.substring(1, trimmed.length - 1).trim();
  } else {
    content = trimmed.substring(1, trimmed.length - 1).trim();
  }

  if (content.isEmpty) return ObjectAst(IMap());

  // Parse properties
  final properties = <String, Ast>{};
  final tokens = _tokenize(content);

  for (var i = 0; i < tokens.length; i++) {
    final token = tokens[i];

    if (token.startsWith(':')) {
      // Property key
      final key = token.substring(1); // Remove :

      if (i + 1 >= tokens.length) {
        // No value for this property
        throw UnpairedPropertyError(key);
      }

      // Parse property value
      final valueToken = tokens[i + 1];
      final valueAst = _parseToken(valueToken);
      if (valueAst == null) return null;

      properties[key] = valueAst;
      i++; // Skip the value token
    } else {
      // This is not a property, it's an atom mixed with properties
      throw MixedContentError(token);
    }
  }

  return ObjectAst(IMap(properties));
}

Ast? _parseSimple(String input) {
  return _parseInteger(input) ??
      _parseFloat(input) ??
      _parseString(input) ??
      _parseSymbol(input);
}

bool _isNumber(String input) {
  return double.tryParse(input) != null;
}

/// Convenience function to parse Glue code
/// Returns either Ast on success or ParserError on failure
Object parseGlue(String input) {
  return GlueParser.parse(input);
}
