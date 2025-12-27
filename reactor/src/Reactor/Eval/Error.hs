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
    | EqExpectedTwoArguments
    | NeExpectedTwoArguments
    | LtExpectedTwoArguments
    | LtExpectedNumbers
    | LeExpectedTwoArguments
    | LeExpectedNumbers
    | GtExpectedTwoArguments
    | GtExpectedNumbers
    | GeExpectedTwoArguments
    | GeExpectedNumbers
    | IfExpectedThreeArguments
    | WhenExpectedAtLeastOneArgument
    | WhileExpectedAtLeastOneArgument
    | UntilExpectedAtLeastOneArgument
    | NotExpectedOneArgument
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
    EqExpectedTwoArguments -> "eq: expected two arguments"
    NeExpectedTwoArguments -> "ne: expected two arguments"
    LtExpectedTwoArguments -> "lt: expected two arguments"
    LtExpectedNumbers -> "lt: expected numbers"
    LeExpectedTwoArguments -> "le: expected two arguments"
    LeExpectedNumbers -> "le: expected numbers"
    GtExpectedTwoArguments -> "gt: expected two arguments"
    GtExpectedNumbers -> "gt: expected numbers"
    GeExpectedTwoArguments -> "ge: expected two arguments"
    GeExpectedNumbers -> "ge: expected numbers"
    IfExpectedThreeArguments -> "if: expected condition, then, and else expressions"
    WhenExpectedAtLeastOneArgument -> "when: expected condition and body"
    WhileExpectedAtLeastOneArgument -> "while: expected condition and body"
    UntilExpectedAtLeastOneArgument -> "until: expected condition and body"
    NotExpectedOneArgument -> "not: expected one argument"
