module Reactor.Eval where

import Control.Monad (ap, liftM)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Reactor.Env qualified as E
import Reactor.Error (ReactorError (..))
import Reactor.Value qualified as V

-- | 1. Конкретизация типов для нашего интерпретатора
newtype Eval a = Eval
    { runEval :: V.Env Eval -> IO (Either ReactorError (a, V.Env Eval))
    }

type Value = V.Value Eval
type Env = V.Env Eval

-- | 2. Стандартные инстансы монады
instance Functor Eval where
    fmap = liftM

instance Applicative Eval where
    pure a = Eval $ \env -> pure $ Right (a, env)
    (<*>) = ap

instance Monad Eval where
    (Eval m) >>= f = Eval $ \env -> do
        res <- m env
        case res of
            Left err -> pure $ Left err
            Right (a, env') -> runEval (f a) env'

-- | 3. Примитивы для работы с окружением (используем Reactor.Env)
getEnv :: Eval Env
getEnv = Eval $ \env -> pure $ Right (env, env)

putEnv :: Env -> Eval ()
putEnv newEnv = Eval $ \_ -> pure $ Right ((), newEnv)

throwError :: ReactorError -> Eval a
throwError err = Eval $ \_ -> pure $ Left err

liftIO :: IO a -> Eval a
liftIO action = Eval $ \env -> do
    a <- action
    pure $ Right (a, env)

-- | 4. Главный цикл интерпретации
eval :: Value -> Eval (Maybe (Value))
-- Самовычисляемые атомы
eval n@(V.VNumber _) = pure $ Just n
eval s@(V.VString _) = pure $ Just s
-- 1. Используем чистый lookupVar из модуля Env
eval (V.VSymbol name) = do
    env <- getEnv
    case E.lookupVar name env of
        Right val -> pure $ Just val
        Left err -> throwError err

-- 2. Список с фильтрацией Nothing
eval (V.VList xs) = do
    results <- mapM eval xs
    let clean = catMaybes results
    case clean of
        -- Динамический вызов: результат вычисления первого элемента — функция
        (f : args) | isCallable f -> apply f args
        -- Просто список данных
        _ -> pure . Just . V.VList $ clean
  where
    isCallable (V.VNative _) = True
    isCallable (V.VClosure{}) = True
    isCallable _ = False

-- 3. Вызов функции
eval (V.VCall name rawArgs) = do
    env <- getEnv
    case E.lookupVar name env of
        Right func -> apply func rawArgs
        Left err -> throwError err

-- 4. Остальные типы
eval v@(V.VObject _) = pure $ Just v
eval v@(V.VClosure{}) = pure $ Just v
eval v@(V.VNative _) = pure $ Just v

-- | 5. Механика вызова (Dispatch)
apply :: Value -> [Value] -> Eval (Maybe Value)
apply (V.VNative native) rawArgs = case native of
    V.VFunc f -> do
        -- Используем eval и catMaybes вместо evalRequired
        args <- catMaybes <$> mapM eval rawArgs
        Just <$> f args
    V.VCmd c -> do
        args <- catMaybes <$> mapM eval rawArgs
        c args >> pure Nothing
    V.VSpecial s -> s rawArgs
apply (V.VClosure params body savedEnv) rawArgs = do
    -- Вычисляем аргументы, игнорируя Nothing
    argValues <- catMaybes <$> mapM eval rawArgs

    if length params /= length argValues
        then throwError (SyntaxError "Wrong number of arguments")
        else do
            -- Вместо ручного Map.fromList используем E.pushFrame + E.defineVar (абстракция!)
            currentEnv <- getEnv
            let newEnv = foldr (\(p, v) e -> E.defineVar p v e) (E.pushFrame savedEnv) (zip params argValues)

            putEnv newEnv
            res <- eval body
            putEnv currentEnv
            pure res

-- Исправляем сообщение об ошибке для соответствия тестам
apply (V.VSymbol name) _ = throwError $ SyntaxError ("Unbound variable: " <> name)
apply _ _ = throwError $ SyntaxError "Not a callable object"

-- | Вспомогательная функция для вычисления обязательных аргументов
evalRequired :: Value -> Eval Value
evalRequired v =
    eval v >>= \case
        Just val -> pure val
        Nothing -> throwError $ SyntaxError "Expected value, but got a command/effect"

-- | Монадическая обертка для определения переменной
defineVarEval :: Text -> Value -> Eval ()
defineVarEval name val = do
    env <- getEnv
    putEnv (E.defineVar name val env)

-- | Монадическая обертка для обновления переменной
updateVarEval :: Text -> Value -> Eval ()
updateVarEval name val = do
    env <- getEnv
    -- Чистая функция updateVar возвращает Either ReactorError Env
    case E.updateVar name val env of
        Right nextEnv -> putEnv nextEnv
        Left err -> throwError err
