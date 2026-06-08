module Glue.Eval.Error (
    EvalError (..),
    Stack,
    prettyShow,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.Eval.Exception (RuntimeException, pretty)

type Stack = [Text]

data EvalError t = EvalError Stack (RuntimeException t)
    deriving (Eq)

instance Show (EvalError m) where
    show (EvalError ctx e) = show ctx ++ ": " ++ show e

prettyShow :: EvalError m -> Text
prettyShow (EvalError ctx e) =
    if null ctx
        then pretty e
        else T.intercalate " -> " (reverse ctx) <> ": " <> pretty e
