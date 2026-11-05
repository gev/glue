module.exports = grammar({
  name: "glue",

  extras: ($) => [/\s/, $.comment],

  conflicts: ($) => [[$.symbol, $.property_key]],

  rules: {
    source_file: ($) => repeat($._expression),

    _expression: ($) =>
      choice(
        $.list,
        $.quoted_expression,
        $.property_key,
        $.boolean,
        $.number,
        $.string,
        $.symbol,
      ),

    list: ($) => seq("(", repeat($._expression), ")"),
    quoted_expression: ($) => seq("'", $._expression),

    property_key: ($) => token(seq(":", /[a-zA-Z0-9_./!?$@#-]*/)),
    boolean: ($) => choice("true", "false"),
    number: ($) =>
      token(
        /[+-]?(?:[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?|\.[0-9]+(?:[eE][+-]?[0-9]+)?)/,
      ),
    string: ($) =>
      seq('"', repeat(choice(/[^"\\\\\n]+/, /\\./)), '"'),
    symbol: ($) =>
      token(/[a-zA-Z_+*/%<>=!&|?$@#.-][a-zA-Z0-9_+*/%<>=!&|?$@#.:\-]*/),
    comment: ($) => token(seq(";", /[^\n]*/)),
  },
});
