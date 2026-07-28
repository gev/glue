module Glue.Lib.Builtin.At (at) where

import Data.List ((!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- | At function - univeral accessor
at :: IR Eval
at = NativeFunc \target ->
    pure $ NativeFunc \accessor ->
        case (target, accessor) of
            (String text, Integer index) ->
                pure
                    if index >= 0 && index < T.length text
                        then String . T.singleton $ T.index text index
                        else Void
            (List list, Integer index) ->
                pure . fromMaybe Void $ list !? index
            (Object object, String field) ->
                pure . fromMaybe Void $ M.lookup field object
            (Object object, Symbol field) ->
                pure . fromMaybe Void $ M.lookup field object
            (Object object, DottedSymbol field) -> nestedLookup field object
            _ -> throwError $ wrongArgumentType ["target", "accessor"]
  where
    nestedLookup [] _ = pure Void
    nestedLookup [key] obj =
        pure . fromMaybe Void $ M.lookup key obj
    nestedLookup (key : rest) obj =
        case M.lookup key obj of
            Just (Object subObj) -> nestedLookup rest subObj
            _ -> pure Void
