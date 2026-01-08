module Glue.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T

data AST where
  String :: Text -> AST
  Integer :: Int -> AST
  Float :: Double -> AST
  Symbol :: Text -> AST
  List :: [AST] -> AST
  Object :: [(Text, AST)] -> AST

instance Show AST where
  show (String s) = "\"" <> T.unpack s <> "\""
  show (Integer n) = show n
  show (Float n) = show n
  show (Symbol s) = T.unpack s
  show (List xs) = "(" <> unwords (map show xs) <> ")"
  show (Object ps) = "(" <> unwords (map showProp ps) <> ")"
   where
    showProp (k, v) = ":" <> T.unpack k <> " " <> show v

instance Eq AST where
  (String a) == (String b) = a == b
  (Integer a) == (Integer b) = a == b
  (Float a) == (Float b) = a == b
  (Symbol a) == (Symbol b) = a == b
  (List a) == (List b) = a == b
  (Object a) == (Object b) = a == b
  _ == _ = False
