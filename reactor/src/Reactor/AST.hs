module Reactor.AST where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T

data RKind = Props | Atoms

data Reactor where
    RExpr :: Text -> RBody k -> Reactor
    RString :: Text -> Reactor
    RNumber :: Scientific -> Reactor
    RSymbol :: Text -> Reactor
    RList :: RBody k -> Reactor

data RBody (k :: RKind) where
    RProps :: [(Text, Reactor)] -> RBody 'Props
    RAtoms :: [Reactor] -> RBody 'Atoms

-- Ручная реализация Show для красивого вывода
instance Show Reactor where
    show (RExpr name body) = "(" ++ T.unpack name ++ " " ++ show body ++ ")"
    show (RString s) = "\"" ++ T.unpack s ++ "\""
    show (RNumber n) = show n
    show (RSymbol s) = T.unpack s
    show (RList body) = "(" ++ show body ++ ")"

instance Show (RBody k) where
    show (RAtoms xs) = unwords (map show xs)
    show (RProps ps) = unwords (map showProp ps)
      where
        showProp (k, v) = ":" ++ T.unpack k ++ " " ++ show v

-- Дополнительно: чтобы можно было сравнивать AST в тестах
instance Eq Reactor where
    (RString a) == (RString b) = a == b
    (RNumber a) == (RNumber b) = a == b
    (RSymbol a) == (RSymbol b) = a == b
    -- Сравнение выражений
    (RExpr n1 b1) == (RExpr n2 b2) = n1 == n2 && compareBodies b1 b2
    -- Сравнение списков
    (RList b1) == (RList b2) = compareBodies b1 b2
    _ == _ = False

-- Вспомогательная функция для сравнения тел с разными индексами k
compareBodies :: RBody k1 -> RBody k2 -> Bool
compareBodies (RAtoms a) (RAtoms b) = a == b
compareBodies (RProps a) (RProps b) = a == b
compareBodies _ _ = False

-- Для самих тел Eq теперь выводится без проблем
deriving instance Eq (RBody k)
