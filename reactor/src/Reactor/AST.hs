module Reactor.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T

data AST where
    String :: Text -> AST
    Number :: Scientific -> AST
    Symbol :: Text -> AST
    AtomList :: [AST] -> AST
    PropList :: [(Text, AST)] -> AST
    PropAccess :: AST -> Text -> AST

instance Show AST where
    show (String s) = "\"" <> T.unpack s <> "\""
    show (Number n) = show n
    show (Symbol s) = T.unpack s
    show (AtomList xs) = "(" <> unwords (map show xs) <> ")"
    show (PropList ps) = "(" <> unwords (map showProp ps) <> ")"
      where
        showProp (k, v) = ":" <> T.unpack k <> " " <> show v
    show (PropAccess obj prop) = "(" <> show obj <> "." <> T.unpack prop <> ")"

instance Eq AST where
    (String a) == (String b) = a == b
    (Number a) == (Number b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (AtomList a) == (AtomList b) = a == b
    (PropList a) == (PropList b) = a == b
    (PropAccess o1 p1) == (PropAccess o2 p2) = o1 == o2 && p1 == p2
    _ == _ = False
