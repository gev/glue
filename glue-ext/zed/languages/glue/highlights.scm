(comment) @comment
(string) @string
(number) @number
(boolean) @boolean
(property_key) @property

(quote) @operator

(list
  (symbol) @function
  .)

(symbol) @variable

((symbol) @keyword
 (#match? @keyword "^(def|set|import|lambda|if|module|export|try|catch|error|quote)$"))

((symbol) @operator
 (#match? @operator "^(\\+|-|\\*|/|%|==|!=|=|<|<=|>|>=|&|\\||!)$"))

("(" ")") @punctuation.bracket
