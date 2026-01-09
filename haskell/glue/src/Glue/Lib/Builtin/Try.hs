module Glue.Lib.Builtin.Try where

import Data.Text (Text)
import Glue.Eval (Eval, apply, eval, getState, isCallable, liftIO, putState, runEval, throwError)
import Glue.Eval.Error (EvalError (..))
import Glue.Eval.Exception
import Glue.IR qualified as IR

tryFunc :: [IR.IR Eval] -> Eval (IR.IR Eval)
tryFunc (body : catches) = do
    state <- getState
    result <- liftIO $ runEval (eval body) state
    case result of
        Right (val, newState) -> do
            putState newState
            pure val
        Left (EvalError _ (RuntimeException sym payload)) -> do
            case findCatch sym catches of
                Just handler -> do
                    callable <- eval handler
                    case callable of
                        c | isCallable c -> apply c (maybe [] (: []) payload)
                        _ -> throwError notCallableObject
                Nothing -> throwError $ RuntimeException sym payload
tryFunc _ = throwError $ wrongArgumentType ["body", "catch*"]

findCatch :: Text -> [IR.IR Eval] -> Maybe (IR.IR Eval)
findCatch _ [] = Nothing
findCatch excSymbol (IR.List [IR.Symbol "catch", catchType, handler] : rest)
    | getSymbolText catchType == Just excSymbol = Just handler
    | otherwise = findCatch excSymbol rest
findCatch excSymbol (_ : rest) = findCatch excSymbol rest

getSymbolText :: IR.IR Eval -> Maybe Text
getSymbolText (IR.Symbol t) = Just t
getSymbolText (IR.String t) = Just t
getSymbolText _ = Nothing
