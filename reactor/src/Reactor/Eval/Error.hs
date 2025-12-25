module Reactor.Eval.Error where

import Data.Text (Text)

data EvalError
    = UnboundVariable Text
    | CanNotSetUnboundVariable Text
    | NotCallableObject
    | ExpectedValue
    | ExpectedListOfSymbols
    | WrongNumberOfArguments
    | PropertyNotFound Text
    | NotAnObject Text
    | DefExpectedSymbolAndValue
    | SetExpectedSymbolAndValue
    | LambdaExpectedArgumentsList
    | LambdaExpectedArgumentsAndBody
    | QuoteExpectedExactlyOneArgument
    deriving (Show, Eq)

prettyShow :: EvalError -> Text
prettyShow = \case
    UnboundVariable name -> "Unbound variable: " <> name
    CanNotSetUnboundVariable name -> "Cannot set unbound variable: " <> name
    NotCallableObject -> "Not a callable object"
    ExpectedValue -> "Expected value, but got a command/effect"
    ExpectedListOfSymbols -> "Expected a list of symbols"
    WrongNumberOfArguments -> "Wrong number of arguments"
    PropertyNotFound prop -> "Property not found: " <> prop
    NotAnObject obj -> "Not an object: " <> obj
    DefExpectedSymbolAndValue -> "def: expected symbol and value"
    SetExpectedSymbolAndValue -> "set: expected symbol and value"
    LambdaExpectedArgumentsList -> "lambda: expected list of arguments"
    LambdaExpectedArgumentsAndBody -> "lambda: expected arguments and body"
    QuoteExpectedExactlyOneArgument -> "quote: expected exactly one argument"
