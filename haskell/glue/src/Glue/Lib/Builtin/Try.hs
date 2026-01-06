module Glue.Lib.Builtin.Try where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.Eval (Eval, apply, eval, isCallable, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR qualified as IR

tryFunc :: [IR.IR Eval] -> Eval (Maybe (IR.IR Eval))
tryFunc (body : catches) = do
    -- Evaluate the body expression
    result <- eval body
    case result of
        Just (IR.Exception excSymbol payload) -> do
            -- Find matching catch clause
            case findCatch excSymbol catches of
                Just handler -> do
                    -- Evaluate handler to get callable
                    callable <- eval handler
                    case callable of
                        Just c | isCallable c -> apply c [payload]
                        _ -> throwError NotCallableObject
                Nothing -> throwError $ RuntimeException excSymbol (T.pack $ show payload)
        Just val -> pure $ Just val
        Nothing -> throwError ExpectedValue
tryFunc _ = throwError $ WrongArgumentType ["body", "catch*"]

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
