module Glue.Lib.List.Range where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

range :: IR Eval
range = Special rangeImpl

rangeImpl :: [IR Eval] -> Eval (IR Eval)
rangeImpl rawArgs = do
    args <- mapM eval rawArgs
    case args of
        [Integer n] -> do
            pure . List $
                Integer
                    <$> if n > 0
                        then [0 .. n - 1]
                        else [0, -1 .. n + 1]
        [Integer start, Integer stop] ->
            pure . List $
                Integer
                    <$> if stop > start
                        then [start .. stop - 1]
                        else [start, start - 1, stop + 1]
        [Integer start, Integer stop, Integer step] -> do
            if step == 0
                then throwError $ wrongArgumentType ["Step shouldn't be zero"]
                else
                    pure . List $
                        Integer
                            <$> if stop > start
                                then [start, start + step .. stop - 1]
                                else [start, start + step .. stop + 1]
        _ ->
            throwError $
                wrongArgumentType
                    [ "Integer n required"
                    , "Integer `start` and Integer `stop` and optional Integer `step` required"
                    ]
