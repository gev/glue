import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'ast.dart';
import 'parser_error.dart';

/// Simple parser for Glue language (basic implementation)
class GlueParser {
  /// Parse a Glue source string into an AST or error
  static Object parse(String input) {
    // Remove comments first
    final cleaned = _removeComments(input.trim());
    if (cleaned.isEmpty) return SyntaxError("Empty input");

    // Try parsing as different AST types
    final result =
        _parseInteger(cleaned) ??
        _parseFloat(cleaned) ??
        _parseString(cleaned) ??
        _parseSymbol(cleaned) ??
        _parseList(cleaned) ??
        _parseObject(cleaned);

    return result ?? SyntaxError("Invalid syntax: '$cleaned'");
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

  // Simple parsing - split by spaces (doesn't handle nested structures yet)
  final elements = content
      .split(RegExp(r'\s+'))
      .where((s) => s.isNotEmpty)
      .map((s) => _parseSimple(s))
      .whereType<Ast>()
      .toList();

  return ListAst(IList(elements));
}

Ast? _parseObject(String input) {
  final trimmed = input.trim();
  if (!trimmed.startsWith('{') || !trimmed.endsWith('}')) return null;

  // Simplified - empty object for now
  return ObjectAst(IMap());
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
