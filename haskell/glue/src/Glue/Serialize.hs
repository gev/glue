module Glue.Serialize (
    serializeAST,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Glue.AST (AST(..))

-- | Serialize AST to Glue string representation
-- Uses Text Builders for efficient conversion
serializeAST :: AST -> Text
serializeAST ast = go ast
  where
    go :: AST -> Builder
    go (Integer n) = decimal n
    go (Float n) = realFloat n
    go (String s) = "\"" <> fromText s <> "\""
    go (Symbol s) = fromText s
    go (List xs) = "(" <> mconcat (map go xs) <> ")"
    go (Object ps) = "(" <> mconcat (map goProp ps) <> ")"
    
    goProp :: (Text, AST) -> Builder
    goProp (k, v) = ":" <> fromText k <> " " <> go v
