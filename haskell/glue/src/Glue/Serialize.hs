module Glue.Serialize (
  serializeAST,
) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Glue.AST (AST (..))

{- | Serialize AST to Glue string representation
Uses Text Builders for efficient conversion
-}
serializeAST :: AST -> Text
serializeAST ast = toLazyText (go ast)
 where
  go (Integer n) = decimal n
  go (Float n) = realFloat n
  go (String s) = "\"" <> fromText s <> "\""
  go (Symbol s) = fromText s
  go (List xs) = "(" <> mconcat (go <$> xs) <> ")"
  go (Object ps) = "(" <> mconcat (goProp <$> ps) <> ")"
  goProp (k, v) = ":" <> fromText k <> " " <> go v
