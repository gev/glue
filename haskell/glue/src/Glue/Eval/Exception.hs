module Glue.Eval.Exception where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.IR (IR (..))

data RuntimeException m
    = RuntimeException Text (Maybe (IR m))
    deriving (Show, Eq)

pretty :: RuntimeException m -> Text
pretty = \case
    RuntimeException sym msg -> "Runtime Exception: " <> sym <> ". " <> T.pack (show msg)

unboundVariable :: Text -> RuntimeException m
unboundVariable = RuntimeException "unbound-variable" . Just . String

canNotSetUnboundVariable :: Text -> RuntimeException m
canNotSetUnboundVariable = RuntimeException "cannot-set-unbound-variable" . Just . String

notCallableObject :: RuntimeException m
notCallableObject = RuntimeException "not-callable-object" Nothing

expectedValue :: RuntimeException m
expectedValue = RuntimeException "expected-value" Nothing

expectedListOfSymbols :: RuntimeException m
expectedListOfSymbols = RuntimeException "expected-list-of-symbols" Nothing

wrongNumberOfArguments :: RuntimeException m
wrongNumberOfArguments = RuntimeException "wrong-number-of-arguments" Nothing

wrongArgumentType :: [Text] -> RuntimeException m
wrongArgumentType expected = RuntimeException "wrong-argument-type" . Just $ List (String <$> expected)

divByZero :: RuntimeException m
divByZero = RuntimeException "div-by-zero" Nothing

propertyNotFound :: Text -> RuntimeException m
propertyNotFound = RuntimeException "property-not-found" . Just . String

notAnObject :: IR m -> RuntimeException m
notAnObject = RuntimeException "not-an-object" . Just

moduleNotFound :: Text -> RuntimeException m
moduleNotFound = RuntimeException "module-not-found" . Just . String

undefinedExport :: Text -> RuntimeException m
undefinedExport = RuntimeException "undefined-export" . Just . String

runtimeException :: Text -> IR m -> RuntimeException m
runtimeException symbol = RuntimeException symbol . Just
