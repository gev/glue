module Reactor.Lib.Math.Error where

import Reactor.Eval.Error (Error (..))

data MathError
    = AddExpectedAtLeastOneArg
    | AddExpectedNumbers
    | SubExpectedAtLeastOneArg
    | SubExpectedNumbers
    | MulExpectedAtLeastOneArg
    | MulExpectedNumbers
    | DivExpectedAtLeastOneArg
    | DivExpectedNumbers
    | DivByZero
    | ModExpectedTwoNumbers
    | ModByZero
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
    deriving (Show, Eq)

instance Error MathError where
    pretty = \case
        AddExpectedAtLeastOneArg -> "+: expected at least one argument"
        AddExpectedNumbers -> "+: expected numbers"
        SubExpectedAtLeastOneArg -> "-: expected at least one argument"
        SubExpectedNumbers -> "-: expected numbers"
        MulExpectedAtLeastOneArg -> "*: expected at least one argument"
        MulExpectedNumbers -> "*: expected numbers"
        DivExpectedAtLeastOneArg -> "/: expected at least one argument"
        DivExpectedNumbers -> "/: expected numbers"
        DivByZero -> "/: division by zero"
        ModExpectedTwoNumbers -> "%: expected two numbers"
        ModByZero -> "%: modulo by zero"
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
