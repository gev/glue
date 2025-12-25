module Reactor.Native (
    initialEnv,
) where

import Reactor.Env qualified as E
import Reactor.Eval (Eval, defineVarEval, evalRequired, getEnv, throwError, updateVarEval)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (Env, IR (..), Native (..))

-- Удобный алиас для конкретного рантайма
type V = IR Eval

-- | Реализация (quote x) -> x
builtinQuote :: [V] -> Eval (Maybe V)
builtinQuote args = case E.makeQuote args of
    Right val -> pure (Just val)
    Left err -> throwError err

-- | Реализация (def symbol value)
builtinDef :: [V] -> Eval (Maybe V)
builtinDef [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Nothing
builtinDef _ = throwError DefExpectedSymbolAndValue

-- | Реализация (set symbol value)
builtinSet :: [V] -> Eval (Maybe V)
builtinSet [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    updateVarEval name val
    pure Nothing
builtinSet _ = throwError SetExpectedSymbolAndValue

-- | Реализация (lambda (arg1 arg2) body)
builtinLambda :: [V] -> Eval (Maybe V)
builtinLambda [argsNode, body] = do
    rawArgs <- case argsNode of
        List xs -> pure xs
        _ -> throwError LambdaExpectedArgumentsList

    params <- case E.extractSymbols rawArgs of
        Right ps -> pure ps
        Left err -> throwError err

    capturedEnv <- getEnv
    pure . Just $ E.makeClosure params body capturedEnv
builtinLambda _ = throwError LambdaExpectedArgumentsAndBody

builtinList :: [V] -> Eval V
builtinList args = pure (List args)

-- | Начальное окружение со всеми встроенными спецформами
initialEnv :: Env Eval
initialEnv =
    E.fromList
        [ ("quote", Native (Special builtinQuote))
        , ("def", Native (Special builtinDef))
        , ("set", Native (Special builtinSet))
        , ("lambda", Native (Special builtinLambda))
        , ("list", Native (Func builtinList)) -- Добавляем сюда
        ]
