module Glue.Eval.Error (
    Error (..),
    EvalError (..),
    GeneralError (..),
    Context,
    prettyShow,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)

type Context = [Text]

class Error e where
    pretty :: e -> Text

data EvalError
    = forall e.
        (Error e, Show e, Eq e, Typeable e) =>
      EvalError Context e

instance Show EvalError where
    show (EvalError ctx e) = show ctx ++ ": " ++ show e

instance Eq EvalError where
    EvalError ctxA a == EvalError ctxB b =
        ctxA == ctxB && case cast a of
            Just a' -> a' == b
            Nothing -> False

data GeneralError
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
    | RuntimeError Text Text
    deriving (Show, Eq)

instance Error GeneralError where
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
        RuntimeError err msg -> "Exception: " <> err <> ". " <> msg

prettyShow :: EvalError -> Text
prettyShow (EvalError ctx e) =
    if null ctx
        then pretty e
        else T.intercalate " -> " (reverse ctx) <> ": " <> pretty e
