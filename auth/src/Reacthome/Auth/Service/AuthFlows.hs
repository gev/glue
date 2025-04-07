module Reacthome.Auth.Service.AuthFlows where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Domain.Challenge
import Reacthome.Auth.Service.AuthFlow

data AuthFlows = AuthFlows
    { start :: AuthFlow -> IO Challenge
    , findBy :: Challenge -> MaybeT IO AuthFlow
    , stop :: Challenge -> IO ()
    }
