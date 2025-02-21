module Reacthome.Assist.Dialog.Gate where

import Control.Monad
import Data.UUID.V4
import Reacthome.Assist.Controller.Dialog.Query
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Prelude hiding (lookup)

getAnswer ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    ) =>
    GetAnswer
getAnswer query = do
    uid <- nextRandom
    sendQuery $ pack uid query
    ?answers.takeAnswer uid

setAnswer ::
    (?answers :: Answers) =>
    SetAnswer
setAnswer uid answer =
    maybe (print $ "Could not put answer with id " <> show uid <> " into answers") pure
        =<< ?answers.putAnswer uid answer
