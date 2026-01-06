module Glue.Eval.Error (
    EvalError (..),
    Context,
    prettyShow,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.Eval.Exception (RuntimeException, pretty)

type Context = [Text]

data EvalError t = EvalError Context (RuntimeException t)
    deriving (Eq)

instance Show (EvalError m) where
    show (EvalError ctx e) = show ctx ++ ": " ++ show e

prettyShow :: EvalError m -> Text
prettyShow (EvalError ctx e) =
    if null ctx
        then pretty e
        else T.intercalate " -> " (reverse ctx) <> ": " <> pretty e
