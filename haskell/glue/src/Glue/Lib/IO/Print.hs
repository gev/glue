module Glue.Lib.IO.Print where

import Data.Text (unpack)
import Glue.Eval (Eval, liftIO)
import Glue.IR (IR (..))

printFunc :: IR Eval
printFunc = NativeFunc printFuncImpl

printFuncImpl :: IR Eval -> Eval (IR Eval)
printFuncImpl (String s) = liftIO (putStr (unpack s)) >> pure Void
printFuncImpl _ = pure Void

println :: IR Eval
println = NativeFunc printlnImpl

printlnImpl :: IR Eval -> Eval (IR Eval)
printlnImpl (String s) = liftIO (putStrLn (unpack s)) >> pure Void
printlnImpl _ = pure Void
