module Reacthome.Auth.Controller.AuthFlowCookie where

import Data.Time
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Web.Cookie

makeAuthFlowCookie ::
    (?environment :: Environment) =>
    Challenge ->
    SetCookie
makeAuthFlowCookie challenge =
    defaultSetCookie
        { setCookieName = "auth_flow_challenge"
        , setCookieValue = challenge.value
        , setCookiePath = Just "/authentication/complete"
        , setCookieMaxAge = Just $ secondsToDiffTime $ fromIntegral ?environment.timeout
        , setCookieSameSite = Just sameSiteStrict
        , setCookieHttpOnly = True
        , setCookieSecure = True
        }
