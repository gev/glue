module Glue.Lib.IO.Print where

import Data.Text (unpack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

printFunc :: [IR Eval] -> Eval ()
printFunc [String s] = liftIO (putStr (unpack s))
printFunc _ = pure ()

println :: [IR Eval] -> Eval ()
println [String s] = liftIO (putStrLn (unpack s))
println _ = pure ()
