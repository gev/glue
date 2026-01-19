module Glue.Lib.IO.Read where

import Data.Text (pack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

readLine :: IR Eval
readLine = NativeFunc readLineImpl

readLineImpl :: [IR Eval] -> Eval (IR Eval)
readLineImpl [] = do
    line <- liftIO getLine
    pure $ String (pack line)
readLineImpl _ = pure $ String ""
