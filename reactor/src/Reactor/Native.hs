module Reactor.Native (
    initialEnv,
) where

import Reactor.Env qualified as E
import Reactor.Error (ReactorError (SyntaxError))
import Reactor.Eval (Eval, defineVarEval, evalRequired, getEnv, throwError, updateVarEval)
import Reactor.Value (Env, Native (..), Value (..))

-- Удобный алиас для конкретного рантайма
type V = Value Eval

-- | Реализация (quote x) -> x
builtinQuote :: [V] -> Eval (Maybe V)
builtinQuote args = case E.makeQuote args of
    Right val -> pure (Just val)
    Left err -> throwError err

-- | Реализация (def symbol value)
builtinDef :: [V] -> Eval (Maybe V)
builtinDef [VSymbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Nothing
builtinDef _ = throwError $ SyntaxError "def: expected symbol and value"

-- | Реализация (set symbol value)
builtinSet :: [V] -> Eval (Maybe V)
builtinSet [VSymbol name, rawVal] = do
    val <- evalRequired rawVal
    updateVarEval name val
    pure Nothing
builtinSet _ = throwError $ SyntaxError "set: expected symbol and value"

-- | Реализация (lambda (arg1 arg2) body)
builtinLambda :: [V] -> Eval (Maybe V)
builtinLambda [argsNode, body] = do
    -- Извлекаем список аргументов независимо от того, VList это или VCall
    rawArgs <- case argsNode of
        VList xs -> pure xs
        VCall n xs -> pure (VSymbol n : xs) -- Если это (x y), то n="x", xs=["y"]
        _ -> throwError $ SyntaxError "lambda: first argument must be a list of parameters"

    params <- case E.extractSymbols rawArgs of
        Right ps -> pure ps
        Left err -> throwError err

    capturedEnv <- getEnv
    pure . Just $ E.makeClosure params body capturedEnv
builtinLambda _ = throwError $ SyntaxError "lambda: expected (lambda (args) body)"

builtinList :: [V] -> Eval V
builtinList args = pure (VList args)

-- | Начальное окружение со всеми встроенными спецформами
initialEnv :: Env Eval
initialEnv =
    E.fromList
        [ ("quote", VNative (VSpecial builtinQuote))
        , ("def", VNative (VSpecial builtinDef))
        , ("set", VNative (VSpecial builtinSet))
        , ("lambda", VNative (VSpecial builtinLambda))
        , ("list", VNative (VFunc builtinList)) -- Добавляем сюда
        ]
