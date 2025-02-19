module Reacthome.Assist.Service.Dialog where

import Reacthome.Assist.Domain.Answer
import Reacthome.Assist.Domain.Query

getAnswer :: Query -> Answer
getAnswer query =
    Answer
        { message = query.message
        , sessionId = query.sessionId
        , endSession = False
        }
