module Reactor.IR where

import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST

type Frame m = Map.Map Text (IR m)
type Env m = [Frame m]

data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | List [IR m]
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
    AST.AtomList xs -> List (map compile xs)
    AST.PropList ps -> Object $ Map.fromList (map (second compile) ps)
    AST.PropAccess obj prop -> PropAccess (compile obj) prop

instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Symbol s -> T.unpack s
        List xs -> "(" <> unwords (map show xs) <> ")"
        PropAccess obj prop -> "(" <> show obj <> "." <> T.unpack prop <> ")"
        Native _ -> "<native>"
        Closure{} -> "<closure>"
        Object _ -> "{object}"

instance Eq (IR m) where
    (Number a) == (Number b) = a == b
    (String a) == (String b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (List a) == (List b) = a == b
    (Object a) == (Object b) = a == b
    (PropAccess o1 p1) == (PropAccess o2 p2) = o1 == o2 && p1 == p2
    _ == _ = False

-- Accessor functions for abstraction
isList :: IR m -> Bool
isList (List _) = True
isList _ = False

listLength :: IR m -> Int
listLength (List xs) = length xs
listLength _ = 0

isObject :: IR m -> Bool
isObject (Object _) = True
isObject _ = False

objectSize :: IR m -> Int
objectSize (Object m) = Map.size m
objectSize _ = 0

objectLookup :: Text -> IR m -> Maybe (IR m)
objectLookup k (Object m) = Map.lookup k m
objectLookup _ _ = Nothing

isSymbol :: IR m -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

getSymbol :: IR m -> Text
getSymbol (Symbol s) = s
getSymbol _ = ""

isPropAccess :: IR m -> Bool
isPropAccess (PropAccess _ _) = True
isPropAccess _ = False

getPropAccess :: IR m -> (IR m, Text)
getPropAccess (PropAccess o p) = (o, p)
getPropAccess _ = error "Not a PropAccess"
