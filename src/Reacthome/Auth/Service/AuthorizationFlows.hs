module Reacthome.Auth.Service.AuthorizationFlows where

import Control.Monad.Trans.Maybe
import Reacthome.Auth.Service.AuthorizationFlow
import Reacthome.Auth.Service.Challenge

data AuthorizationFlows = AuthorizationFlows
    { startCredentialGrantFlow :: IO Challenge
    , startAuthorizationCodeGrantFlow :: Scope -> State -> RedirectUri -> ClientId -> IO Challenge
    , findBy :: Challenge -> MaybeT IO AuthorizationFlow
    , stop :: Challenge -> IO ()
    }
