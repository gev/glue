module Reactor.Lib.Bool.Error where

import Reactor.Eval.Error (Error (..))

data BoolError
    = EqExpectedTwoArgs
    | NeExpectedTwoArgs
    | LtExpectedTwoArgs
    | LtExpectedNumbers
    | LeExpectedTwoArgs
    | LeExpectedNumbers
    | GtExpectedTwoArgs
    | GtExpectedNumbers
    | GeExpectedTwoArgs
    | GeExpectedNumbers
    | NotExpectedOneArg
    | IfExpectedThreeArgs
    | WhenExpectedAtLeastOneArg
    | WhileExpectedAtLeastOneArg
    | UntilExpectedAtLeastOneArg
    deriving (Show, Eq)

instance Error BoolError where
    pretty = \case
        EqExpectedTwoArgs -> "eq: expected two arguments"
        NeExpectedTwoArgs -> "ne: expected two arguments"
        LtExpectedTwoArgs -> "lt: expected two arguments"
        LtExpectedNumbers -> "lt: expected numbers"
        LeExpectedTwoArgs -> "le: expected two arguments"
        LeExpectedNumbers -> "le: expected numbers"
        GtExpectedTwoArgs -> "gt: expected two arguments"
        GtExpectedNumbers -> "gt: expected numbers"
        GeExpectedTwoArgs -> "ge: expected two arguments"
        GeExpectedNumbers -> "ge: expected numbers"
        NotExpectedOneArg -> "not: expected one argument"
        IfExpectedThreeArgs -> "if: expected condition, then, and else expressions"
        WhenExpectedAtLeastOneArg -> "when: expected condition and body"
        WhileExpectedAtLeastOneArg -> "while: expected condition and body"
        UntilExpectedAtLeastOneArg -> "until: expected condition and body"
