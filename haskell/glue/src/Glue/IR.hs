module Glue.IR where

import Data.Bifunctor (second)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Glue.AST (AST)
import Glue.AST qualified as AST

type Frame m = Map.Map Text (IR m)
type Env m = [Frame m]

data IR m
    = Number Scientific
    | String Text
    | Bool Bool
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Module (Map Text (IR m))
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
    AST.Symbol s ->
        if T.isInfixOf "." s
            then DottedSymbol (T.splitOn "." s)
            else Symbol s
    AST.List xs -> List (map compile xs)
    AST.Object ps -> Object $ Map.fromList (map (second compile) ps)

instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Bool True -> "true"
        Bool False -> "false"
        Symbol s -> T.unpack s
        DottedSymbol parts -> T.unpack (T.intercalate "." parts)
        List xs -> "(" <> unwords (map show xs) <> ")"
        Object _ -> "{object}"
        Module _ -> "{module}"
        Native _ -> "<native>"
        Closure{} -> "<closure>"

instance Eq (IR m) where
    Number a == Number b = a == b
    String a == String b = a == b
    Bool a == Bool b = a == b
    Symbol a == Symbol b = a == b
    DottedSymbol a == DottedSymbol b = a == b
    List a == List b = a == b
    Object a == Object b = a == b
    Module a == Module b = a == b
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
isSymbol (DottedSymbol _) = True
isSymbol _ = False

getSymbol :: IR m -> Text
getSymbol (Symbol s) = s
getSymbol (DottedSymbol parts) = T.intercalate "." parts
getSymbol _ = ""
