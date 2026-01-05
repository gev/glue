module Glue.Lib.IO.Read where

import Data.Text (pack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

readLine :: [IR Eval] -> Eval (IR Eval)
readLine [] = do
    line <- liftIO getLine
    pure $ String (pack line)
readLine _ = pure $ String ""
