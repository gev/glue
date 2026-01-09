module Glue.Lib.IO.Print where

import Data.Text (unpack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

printFunc :: [IR Eval] -> Eval (IR Eval)
printFunc [String s] = liftIO (putStr (unpack s)) >> pure Void
printFunc _ = pure Void

println :: [IR Eval] -> Eval (IR Eval)
println [String s] = liftIO (putStrLn (unpack s)) >> pure Void
println _ = pure Void
