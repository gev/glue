module Glue.Lib.Builtin.List (list) where

import Glue.Eval (Eval, eval)
import Glue.IR (IR (..))

-- | List special form - returns its a list of evaluated arguments
list :: IR Eval
list = Special listImpl

listImpl :: [IR Eval] -> Eval (IR Eval)
listImpl xs = List <$> mapM eval xs
