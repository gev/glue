module Reacthome.Assist.Dialog.Gate where

import Control.Monad
import Data.Maybe
import Data.UUID.V4
import Reacthome.Assist.Controller.Dialog.Query
import Reacthome.Assist.Domain.Server.Id
import Reacthome.Assist.Service.Dialog
import Reacthome.Gate.Connection.Pool
import Prelude hiding (lookup)

getAnswer ::
    ( ?answers :: Answers
    , ?gateConnectionPool :: GateConnectionPool
    ) =>
    ServerId ->
    GetAnswer
getAnswer sid query = do
    uid <- nextRandom
    sendQuery sid $ pack uid query
    ?answers.takeAnswer uid

setAnswer ::
    (?answers :: Answers) =>
    SetAnswer
setAnswer uid answer =
    maybe (print $ "Could not put answer with id " <> show uid <> " into answers") pure
        =<< ?answers.putAnswer uid answer
