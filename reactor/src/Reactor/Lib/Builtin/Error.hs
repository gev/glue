module Reactor.Lib.Builtin.Error where

import Reactor.Eval.Error (Error (..))

data BuiltinError
    = DefExpectedSymbolAndValue
    | SetExpectedSymbolAndValue
    | LambdaExpectedArgumentsList
    | LambdaExpectedArgumentsAndBody
    | QuoteExpectedExactlyOneArgument
    deriving (Show, Eq)

instance Error BuiltinError where
    pretty = \case
        DefExpectedSymbolAndValue -> "def: expected symbol and value"
        SetExpectedSymbolAndValue -> "set: expected symbol and value"
        LambdaExpectedArgumentsList -> "lambda: expected list of arguments"
        LambdaExpectedArgumentsAndBody -> "lambda: expected arguments and body"
        QuoteExpectedExactlyOneArgument -> "quote: expected exactly one argument"
