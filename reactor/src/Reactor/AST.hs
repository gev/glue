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

data Body (k :: BodyKind) where
    Props :: [(Text, AST)] -> Body 'PropsKind
    Atoms :: [AST] -> Body 'AtomsKind

-- Ручная реализация Show для красивого вывода
instance Show AST where
    show (String s) = "\"" ++ T.unpack s ++ "\""
    show (Number n) = show n
    show (Symbol s) = T.unpack s
    show (List body) = "(" ++ show body ++ ")"

instance Show (Body k) where
    show (Atoms xs) = unwords (map show xs)
    show (Props ps) = unwords (map showProp ps)
      where
        showProp (k, v) = ":" ++ T.unpack k ++ " " ++ show v

-- Дополнительно: чтобы можно было сравнивать AST в тестах
instance Eq AST where
    (String a) == (String b) = a == b
    (Number a) == (Number b) = a == b
    (Symbol a) == (Symbol b) = a == b
    -- Сравнение списков
    (List b1) == (List b2) = compareBodies b1 b2
    _ == _ = False

-- Вспомогательная функция для сравнения тел с разными индексами k
compareBodies :: Body k1 -> Body k2 -> Bool
compareBodies (Atoms a) (Atoms b) = a == b
compareBodies (Props a) (Props b) = a == b
compareBodies _ _ = False

-- Для самих тел Eq теперь выводится без проблем
deriving instance Eq (Body k)
