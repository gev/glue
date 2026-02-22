module Glue.Serialize (
  serializeAST,
) where

import Data.List (intersperse)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (fromText, singleton, toLazyText)
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
  go (List xs) = "(" <> goList (go <$> xs) <> ")"
  go (Object ps) = "(" <> goList (goProp <$> ps) <> ")"
  goList = mconcat . intersperse (singleton ' ')
  goProp (k, v) = singleton ':' <> fromText k <> singleton ' ' <> go v
