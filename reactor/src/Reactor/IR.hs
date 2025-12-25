module Reactor.IR where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST

type Env m = [Map Text (IR m)]

data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | List [IR m]
    | Object (Map Text (IR m))
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
    AST.List body -> List (compileBody body)

compileBody :: AST.Body k -> [IR m]
compileBody = \case
    AST.Atoms xs -> map compile xs
    AST.Props ps -> concatMap (\(k, v) -> [Symbol (":" <> k), compile v]) ps

instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Symbol s -> T.unpack s
        List xs -> "(" <> unwords (map show xs) <> ")"
        Native _ -> "<native>"
        Closure{} -> "<closure>"
        Object _ -> "{object}"

instance Eq (IR m) where
    (Number a) == (Number b) = a == b
    (String a) == (String b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (List a) == (List b) = a == b
    (Object a) == (Object b) = a == b
    _ == _ = False
