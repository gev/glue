module Reactor.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T

data BodyKind = PropsKind | AtomsKind

data AST where
    String :: Text -> AST
    Number :: Scientific -> AST
    Symbol :: Text -> AST
    List :: Body k -> AST
    PropAccess :: AST -> Text -> AST

data Body (k :: BodyKind) where
    Props :: [(Text, AST)] -> Body 'PropsKind
    Atoms :: [AST] -> Body 'AtomsKind

instance Show AST where
    show (String s) = "\"" <> T.unpack s <> "\""
    show (Number n) = show n
    show (Symbol s) = T.unpack s
    show (List body) = "(" <> show body <> ")"
    show (PropAccess obj prop) = "(" <> show obj <> "." <> T.unpack prop <> ")"

instance Show (Body k) where
    show (Atoms xs) = unwords (map show xs)
    show (Props ps) = unwords (map showProp ps)
      where
        showProp (k, v) = ":" <> T.unpack k <> " " <> show v

instance Eq AST where
    (String a) == (String b) = a == b
    (Number a) == (Number b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (List b1) == (List b2) = compareBodies b1 b2
    (PropAccess o1 p1) == (PropAccess o2 p2) = o1 == o2 && p1 == p2
    _ == _ = False

compareBodies :: Body k1 -> Body k2 -> Bool
compareBodies (Atoms a) (Atoms b) = a == b
compareBodies (Props a) (Props b) = a == b
compareBodies _ _ = False

deriving instance Eq (Body k)
