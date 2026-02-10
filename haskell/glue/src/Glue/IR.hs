module Glue.IR (
    -- Main types
    IR (..),
    Value (..),
    -- Environment types
    Frame,
    Env,
    -- Utility functions
    hostValue,
    hostValueWithProps,
    extractValue,
    isValue,
    getValueFromIR,
    -- Compilation
    compile,
    -- Accessor functions
    isList,
    listLength,
    isObject,
    objectSize,
    objectLookup,
    isSymbol,
    getSymbol,
) where

import Data.Bifunctor (second)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Glue.AST (AST)
import Glue.AST qualified as AST

type Frame m = Map.Map Text (IR m)
type Env m = [Frame m]

-- Host value wrapper for any host language object
data Value m = Value
    { getValue :: Dynamic
    , getters :: Map Text (m (IR m))
    }

instance Show (Value m) where
    show (Value _ getters) =
        "<host-value getters:"
            <> show (Map.size getters)
            <> ">"

instance Eq (Value m) where
    -- Host values are opaque, so we can't meaningfully compare them
    -- For now, consider them never equal (could be improved with identity comparison)
    _ == _ = False

-- Create a host value from any typeable value
hostValue :: (Typeable a) => a -> Value m
hostValue x = Value (toDyn x) Map.empty

-- Create a host value with properties
hostValueWithProps :: (Typeable a) => a -> Map Text (m (IR m)) -> Value m
hostValueWithProps x = Value (toDyn x)

-- Extract a host value with type safety
extractValue :: (Typeable a) => Value m -> Maybe a
extractValue = fromDynamic . getValue

data IR m
    = Integer Int
    | Float Double
    | String Text
    | Bool Bool
    | Symbol Text
    | DottedSymbol [Text]
    | List [IR m]
    | Object (Map Text (IR m))
    | Void
    | Evaluable (m (IR m)) -- Functions without arguments
    | NativeValue (Value m) -- Host language values (literals)
    | NativeFunc (IR m -> m (IR m)) -- Curred functions with arguments
    | Special ([IR m] -> m (IR m)) -- Special forms
    | Closure [Text] (IR m) (Env m)

-- Show instance for IR handles NativeFunc and Special directly

compile :: AST -> IR m
compile = \case
    AST.Integer n -> Integer n
    AST.Float n -> Float n
    AST.String s -> String s
    AST.Symbol s ->
        if T.isInfixOf "." s
            then DottedSymbol (T.splitOn "." s)
            else Symbol s
    AST.List xs -> List (map compile xs)
    AST.Object ps -> Object $ Map.fromList (map (second compile) ps)

instance Show (IR m) where
    show = \case
        Integer n -> show n
        Float n -> show n
        String s -> "\"" <> T.unpack s <> "\""
        Bool True -> "true"
        Bool False -> "false"
        Symbol s -> T.unpack s
        DottedSymbol parts -> T.unpack (T.intercalate "." parts)
        List xs -> "(" <> unwords (map show xs) <> ")"
        Object props -> "(" <> unwords (map (\(k, v) -> ":" <> T.unpack k <> " " <> show v) (Map.toList props)) <> ")"
        Void -> "#<void>"
        NativeValue hv -> "<host:" <> show hv <> ">"
        Evaluable _ -> "<evaluable>"
        NativeFunc _ -> "<native-func>"
        Special _ -> "<special>"
        Closure{} -> "<closure>"

instance Eq (IR m) where
    Integer a == Integer b = a == b
    Float a == Float b = a == b
    String a == String b = a == b
    Bool a == Bool b = a == b
    Symbol a == Symbol b = a == b
    DottedSymbol a == DottedSymbol b = a == b
    List a == List b = a == b
    Object a == Object b = a == b
    NativeValue a == NativeValue b = a == b
    Void == Void = True
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

-- Host value utilities
isValue :: IR m -> Bool
isValue (NativeValue _) = True
isValue _ = False

getValueFromIR :: IR m -> Maybe (Value m)
getValueFromIR (NativeValue hv) = Just hv
getValueFromIR _ = Nothing
