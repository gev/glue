module Reacthome.Auth.Service.AuthFlows where

import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Service.AuthFlow

data AuthFlows = AuthFlows
    { start :: AuthFlow -> IO Challenge
    , findBy :: Challenge -> IO (Either String AuthFlow)
    , stop :: Challenge -> IO ()
    }
