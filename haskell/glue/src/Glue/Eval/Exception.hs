module Glue.Eval.Exception (
    Exception (..),
    EvalError (..),
    RuntimeException (..),
    Context,
) where

import Data.Text (Text)
import Data.Text qualified as T

type Context = [Text]

class Exception e where
    pretty :: e -> Text
    symbol :: e -> Text

data EvalError = EvalError Context RuntimeException
    deriving (Eq)

instance Show EvalError where
    show (EvalError ctx e) = show ctx ++ ": " ++ show e

data RuntimeException
    = UnboundVariable Text
    | CanNotSetUnboundVariable Text
    | NotCallableObject
    | ExpectedValue
    | ExpectedListOfSymbols
    | WrongNumberOfArguments
    | WrongArgumentType [Text]
    | DivByZero
    | PropertyNotFound Text
    | NotAnObject Text
    | ModuleNotFound Text
    | CannotModifyModule
    | RuntimeException Text Text
    deriving (Show, Eq)

instance Exception RuntimeException where
    pretty = \case
        UnboundVariable name -> "Unbound variable: " <> name
        CanNotSetUnboundVariable name -> "Cannot set unbound variable: " <> name
        NotCallableObject -> "Not a callable object"
        ExpectedValue -> "Expected value, but got a command/effect"
        ExpectedListOfSymbols -> "Expected a list of symbols"
        WrongNumberOfArguments -> "Wrong number of arguments"
        WrongArgumentType expected -> "Expected [" <> T.intercalate ", " expected <> "]"
        DivByZero -> "Division by zero"
        PropertyNotFound prop -> "Property not found: " <> prop
        NotAnObject obj -> "Not an object: " <> obj
        ModuleNotFound name -> "Module not found: " <> name
        CannotModifyModule -> "Cannot modify module properties - modules are immutable"
        RuntimeException sym msg -> "Runtime Exception: " <> sym <> ". " <> T.pack (show msg)

    symbol = \case
        UnboundVariable _ -> "unbound-variable"
        CanNotSetUnboundVariable _ -> "cannot-set-unbound-variable"
        NotCallableObject -> "not-callable-object"
        ExpectedValue -> "expected-value"
        ExpectedListOfSymbols -> "expected-list-of-symbols"
        WrongNumberOfArguments -> "wrong-number-of-arguments"
        WrongArgumentType _ -> "wrong-argument-type"
        DivByZero -> "div-by-zero"
        PropertyNotFound _ -> "property-not-found"
        NotAnObject _ -> "not-an-object"
        ModuleNotFound _ -> "module-not-found"
        CannotModifyModule -> "cannot-modify-module"
        RuntimeException _ _ -> "runtime-exception"
