module Reacthome.Auth.Service.AuthFlows where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.Challenge

data AuthFlows = AuthFlows
    { startFlow :: AuthFlow -> IO Challenge
    , findBy :: Challenge -> MaybeT IO AuthFlow
    , stop :: Challenge -> IO ()
    }
