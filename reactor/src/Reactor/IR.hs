module Reactor.IR where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Reactor.AST (AST)
import Reactor.AST qualified as AST

-- Он тоже параметризован монадой m, так как хранит IR m.
type Env m = [Map Text (IR m)]

-- | Рантайм-значение, параметризованное монадой исполнения 'm'
data IR m
    = Number Scientific
    | String Text
    | Symbol Text
    | List [IR m]
    | Object (Map Text (IR m))
    | Call Text [IR m]
    | Native (Native m)
    | Closure [Text] (IR m) (Env m) -- Тело теперь тоже IR m

-- | Типы нативных функций
data Native m
    = Func ([IR m] -> m (IR m))
    | Cmd ([IR m] -> m ())
    | Special ([IR m] -> m (Maybe (IR m)))

-- =============================================================================
-- ТРАНСФОРМАЦИЯ: AST -> IR m
-- =============================================================================

{- | Превращает дерево AST в дерево IR.
Эта функция универсальна для любой монады m.
-}
prepare :: AST -> IR m
prepare = \case
    AST.Number n -> Number n
    AST.String s -> String s
    AST.Symbol s -> Symbol s
    AST.List body -> List (prepareBody body)
    AST.Expr name body -> Call name (prepareBody body)

-- | Вспомогательная функция для обработки тел списков/выражений
prepareBody :: AST.Body k -> [IR m]
prepareBody = \case
    AST.Atoms xs -> map prepare xs
    -- Свойства разворачиваем в плоский список: [:key, val, :key2, val2]
    AST.Props ps -> concatMap (\(k, v) -> [Symbol (":" <> k), prepare v]) ps

-- Экземпляр Show для отладки
instance Show (IR m) where
    show = \case
        Number n -> show n
        String s -> "\"" ++ T.unpack s ++ "\""
        Symbol s -> T.unpack s
        List xs -> "(" ++ unwords (map show xs) ++ ")"
        Call n as -> "(" ++ T.unpack n ++ " " ++ unwords (map show as) ++ ")"
        Native _ -> "<native>"
        Closure{} -> "<closure>"
        Object _ -> "{object}"

instance Eq (IR m) where
    (Number a) == (Number b) = a == b
    (String a) == (String b) = a == b
    (Symbol a) == (Symbol b) = a == b
    (List a) == (List b) = a == b
    (Call n1 a1) == (Call n2 a2) = n1 == n2 && a1 == a2
    (Object a) == (Object b) = a == b
    -- Функции и замыкания считаем неравными в целях тестирования данных
    _ == _ = False
