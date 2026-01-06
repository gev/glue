module Glue.Eval.Error (
    EvalError (..),
    Context,
    prettyShow,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.Eval.Exception (Exception (pretty), RuntimeException)

type Context = [Text]

data EvalError = EvalError Context RuntimeException
    deriving (Eq)

instance Show EvalError where
    show (EvalError ctx e) = show ctx ++ ": " ++ show e

prettyShow :: EvalError -> Text
prettyShow (EvalError ctx e) =
    if null ctx
        then pretty e
        else T.intercalate " -> " (reverse ctx) <> ": " <> pretty e
