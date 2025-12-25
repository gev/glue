module Reactor.IR where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST

type Env m = [Map.Map Text (IR m)]

data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | AtomList [IR m]
    | PropList [(Text, IR m)]
    | Object (Map Text (IR m))
    | PropAccess (IR m) Text
    | Native (Native m)
    | Closure [Text] (IR m) (Env m)

data Native m
    = Func ([IR m] -> m (IR m))
    | Cmd ([IR m] -> m ())
    | Special ([IR m] -> m (Maybe (IR m)))

compile :: AST -> IR m
compile = \case
    AST.Number n -> Number n
    AST.String s -> String s
    AST.Symbol s -> Symbol s
    AST.AtomList xs -> AtomList (map compile xs)
    AST.PropList ps -> PropList (map (\(k, v) -> (k, compile v)) ps)
    AST.PropAccess obj prop -> PropAccess (compile obj) prop

instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Symbol s -> T.unpack s
        AtomList xs -> "(" <> unwords (map show xs) <> ")"
        PropList ps -> "{" <> unwords (map (\(k, v) -> T.unpack k <> ": " <> show v) ps) <> "}"
        PropAccess obj prop -> "(" <> show obj <> "." <> T.unpack prop <> ")"
        Native _ -> "<native>"
        Closure{} -> "<closure>"
        Object _ -> "{object}"

instance Eq (IR m) where
    (Number a) == (Number b) = a == b
    (String a) == (String b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (AtomList a) == (AtomList b) = a == b
    (PropList a) == (PropList b) = a == b
    (Object a) == (Object b) = a == b
    (PropAccess o1 p1) == (PropAccess o2 p2) = o1 == o2 && p1 == p2
    _ == _ = False
