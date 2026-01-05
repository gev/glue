module Glue.Lib.Builtin.Try where

import Data.Text (Text)
import Data.Text qualified as T
import Glue.Eval (Eval, apply, eval, throwError)
import Glue.Eval.Error (GeneralError (..))
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
                    -- Apply handler with payload
                    apply handler [payload]
                Nothing -> throwError $ RuntimeError excSymbol (T.pack $ show payload)
        Just val -> pure $ Just val
        Nothing -> throwError ExpectedValue
tryFunc _ = throwError $ WrongArgumentType ["body", "catch*"]

findCatch :: Text -> [IR.IR Eval] -> Maybe (IR.IR Eval)
findCatch _ [] = Nothing
findCatch excSymbol (IR.List [IR.Symbol "catch", IR.Symbol catchSymbol, handler] : rest)
    | excSymbol == catchSymbol = Just handler
    | otherwise = findCatch excSymbol rest
findCatch excSymbol (_ : rest) = findCatch excSymbol rest
