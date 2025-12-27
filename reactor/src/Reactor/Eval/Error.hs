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
    | SinExpectedOneNumber
    | CosExpectedOneNumber
    | TanExpectedOneNumber
    | AsinExpectedOneNumber
    | AcosExpectedOneNumber
    | AtanExpectedOneNumber
    | AbsExpectedOneNumber
    | SqrtExpectedOneNumber
    | ExpExpectedOneNumber
    | LogExpectedPositiveNumber
    | PowExpectedTwoNumbers
    | FloorExpectedOneNumber
    | CeilExpectedOneNumber
    | MinExpectedTwoNumbers
    | MaxExpectedTwoNumbers
    | RoundExpectedOneNumber
    | TruncExpectedOneNumber
    | AddExpectedAtLeastOneArgument
    | AddExpectedNumbers
    | SubExpectedAtLeastOneArgument
    | SubExpectedNumbers
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
    SinExpectedOneNumber -> "sin: expected one number"
    CosExpectedOneNumber -> "cos: expected one number"
    TanExpectedOneNumber -> "tan: expected one number"
    AsinExpectedOneNumber -> "asin: expected one number"
    AcosExpectedOneNumber -> "acos: expected one number"
    AtanExpectedOneNumber -> "atan: expected one number"
    AbsExpectedOneNumber -> "abs: expected one number"
    SqrtExpectedOneNumber -> "sqrt: expected one number"
    ExpExpectedOneNumber -> "exp: expected one number"
    LogExpectedPositiveNumber -> "log: expected positive number"
    PowExpectedTwoNumbers -> "pow: expected two numbers"
    FloorExpectedOneNumber -> "floor: expected one number"
    CeilExpectedOneNumber -> "ceil: expected one number"
    MinExpectedTwoNumbers -> "min: expected two numbers"
    MaxExpectedTwoNumbers -> "max: expected two numbers"
    RoundExpectedOneNumber -> "round: expected one number"
    TruncExpectedOneNumber -> "trunc: expected one number"
    AddExpectedAtLeastOneArgument -> "+: expected at least one argument"
    AddExpectedNumbers -> "+: expected numbers"
    SubExpectedAtLeastOneArgument -> "-: expected at least one argument"
    SubExpectedNumbers -> "-: expected numbers"
