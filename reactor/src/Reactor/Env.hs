module Reactor.Env where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Reactor.Error (ReactorError (..))
import Reactor.Value (Env, Value (..))

-- Пустое окружение
emptyEnv :: Env m
emptyEnv = [Map.empty]

-- | Создает окружение из плоского списка (удобно для тестов)
fromList :: [(Text, Value m)] -> Env m
fromList pairs = [Map.fromList pairs]

-- | Создает новый локальный уровень (например, при входе в функцию)
pushFrame :: Env m -> Env m
pushFrame env = Map.empty : env

{- | Удаляет текущий уровень (при выходе из функции)
Если уровней больше нет, возвращает пустой стек
-}
popFrame :: Env m -> Env m
popFrame (_ : fs) = fs
popFrame [] = []

-- | Проверяет переменную ТОЛЬКО в текущем фрейме (без поиска вглубь)
lookupLocal :: Text -> Env m -> Maybe (Value m)
lookupLocal name (f : _) = Map.lookup name f
lookupLocal _ [] = Nothing

-- Поиск (от локального к глобальному)
lookupVar :: Text -> Env m -> Either ReactorError (Value m)
lookupVar name [] = Left $ SyntaxError ("Unbound variable: " <> name)
lookupVar name (f : fs) = case Map.lookup name f of
    Just val -> Right val
    Nothing -> lookupVar name fs

-- Определение (всегда в верхний фрейм)
defineVar :: Text -> Value m -> Env m -> Env m
defineVar name val [] = [Map.singleton name val]
defineVar name val (f : fs) = Map.insert name val f : fs

-- Обновление (там, где найдено)
updateVar :: Text -> Value m -> Env m -> Either ReactorError (Env m)
updateVar name _ [] = Left $ SyntaxError ("Cannot set unbound variable: " <> name)
updateVar name val (f : fs)
    | Map.member name f = Right (Map.insert name val f : fs)
    | otherwise = (f :) <$> updateVar name val fs

{- | Чистая логика создания замыкания.
Она просто упаковывает параметры, тело и текущий снимок окружения.
-}
makeClosure :: [Text] -> Value m -> Env m -> Value m
makeClosure = VClosure

{- | Чистая логика цитирования.
Отделяем проверку аргументов от монады Eval.
-}
makeQuote :: [Value m] -> Either ReactorError (Value m)
makeQuote [v] = Right v
makeQuote _ = Left $ SyntaxError "quote: expected exactly 1 argument"

{- | Хелпер для извлечения имен аргументов.
Полезен для подготовки данных перед созданием замыкания.
-}
extractSymbols :: [Value m] -> Either ReactorError [Text]
extractSymbols = mapM \case
    VSymbol s -> Right s
    _ -> Left $ SyntaxError "Expected a list of symbols for arguments"
