module Reactor.Eval.Error (
    Error (..),
    EvalError (..),
    GeneralError (..),
    prettyShow,
) where

import Data.Text (Text)
import Data.Typeable (Typeable, cast)

class Error e where
    pretty :: e -> Text

data EvalError = forall e. (Error e, Show e, Eq e, Typeable e) => EvalError e

instance Show EvalError where
    show (EvalError e) = show e

instance Eq EvalError where
    EvalError a == EvalError b = case cast a of
        Just a' -> a' == b
        Nothing -> False

data GeneralError
    = UnboundVariable Text
    | CanNotSetUnboundVariable Text
    | NotCallableObject
    | ExpectedValue
    | ExpectedListOfSymbols
    | WrongNumberOfArguments
    | PropertyNotFound Text
    | NotAnObject Text
    deriving (Show, Eq, Typeable)

instance Error GeneralError where
    pretty = \case
        UnboundVariable name -> "Unbound variable: " <> name
        CanNotSetUnboundVariable name -> "Cannot set unbound variable: " <> name
        NotCallableObject -> "Not a callable object"
        ExpectedValue -> "Expected value, but got a command/effect"
        ExpectedListOfSymbols -> "Expected a list of symbols"
        WrongNumberOfArguments -> "Wrong number of arguments"
        PropertyNotFound prop -> "Property not found: " <> prop
        NotAnObject obj -> "Not an object: " <> obj

prettyShow :: EvalError -> Text
prettyShow (EvalError e) = pretty e
