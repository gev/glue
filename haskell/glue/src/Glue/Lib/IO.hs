module Glue.Lib.IO where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.IO.Print (printFunc, println)
import Glue.Lib.IO.Read (readLine)

io :: Frame Eval
io =
    E.frameFromList
        [ ("print", Native (Func printFunc))
        , ("println", Native (Func println))
        , ("read-line", Native (Func readLine))
        ]
