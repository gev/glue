module Reactor.Value where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST

-- Он тоже параметризован монадой m, так как хранит Value m.
type Env m = [Map Text (Value m)]

-- | Рантайм-значение, параметризованное монадой исполнения 'm'
data Value m
    = VNumber Scientific
    | VString Text
    | VSymbol Text
    | VList [Value m]
    | VObject (Map Text (Value m))
    | VCall Text [Value m]
    | VNative (Native m)
    | VClosure [Text] (Value m) (Env m) -- Тело теперь тоже Value m

-- | Типы нативных функций
data Native m
    = VFunc ([Value m] -> m (Value m))
    | VCmd ([Value m] -> m ())
    | VSpecial ([Value m] -> m (Maybe (Value m)))

-- =============================================================================
-- ТРАНСФОРМАЦИЯ: AST -> Value m
-- =============================================================================

{- | Превращает дерево AST в дерево Value.
Эта функция универсальна для любой монады m.
-}
prepare :: AST -> Value m
prepare = \case
    AST.Number n -> VNumber n
    AST.String s -> VString s
    AST.Symbol s -> VSymbol s
    AST.List body -> VList (prepareBody body)
    AST.Expr name body -> VCall name (prepareBody body)

-- | Вспомогательная функция для обработки тел списков/выражений
prepareBody :: AST.Body k -> [Value m]
prepareBody = \case
    AST.Atoms xs -> map prepare xs
    -- Свойства разворачиваем в плоский список: [:key, val, :key2, val2]
    AST.Props ps -> concatMap (\(k, v) -> [VSymbol (":" <> k), prepare v]) ps

-- Экземпляр Show для отладки
instance Show (Value m) where
    show = \case
        VNumber n -> show n
        VString s -> "\"" ++ T.unpack s ++ "\""
        VSymbol s -> T.unpack s
        VList xs -> "(" ++ unwords (map show xs) ++ ")"
        VCall n as -> "(" ++ T.unpack n ++ " " ++ unwords (map show as) ++ ")"
        VNative _ -> "<native>"
        VClosure{} -> "<closure>"
        VObject _ -> "{object}"

instance Eq (Value m) where
    (VNumber a) == (VNumber b) = a == b
    (VString a) == (VString b) = a == b
    (VSymbol a) == (VSymbol b) = a == b
    (VList a) == (VList b) = a == b
    (VCall n1 a1) == (VCall n2 a2) = n1 == n2 && a1 == a2
    (VObject a) == (VObject b) = a == b
    -- Функции и замыкания считаем неравными в целях тестирования данных
    _ == _ = False
