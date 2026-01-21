module Glue.Lib.IO.Read where

import Data.Text (pack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

readLine :: IR Eval
readLine = NativeFunc readLineImpl

readLineImpl :: IR Eval -> Eval (IR Eval)
readLineImpl Void = do
    -- Accept Void as dummy argument for nullary call
    line <- liftIO getLine
    pure $ String (pack line)
readLineImpl _ = pure $ String ""
