module Reacthome.Auth.Controller.AuthFlowCookie where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString
import Data.ByteString.Base64
import Data.Time
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.Challenge
import Web.Cookie
import Web.Rest

cookieName :: ByteString
cookieName = "auth_flow_challenge"

makeAuthFlowCookie ::
    (?environment :: Environment) =>
    Challenge ->
    SetCookie
makeAuthFlowCookie challenge =
    defaultSetCookie
        { setCookieName = cookieName
        , setCookieValue = encode challenge.value
        , setCookiePath = Just "/authentication/complete"
        , setCookieMaxAge = Just $ secondsToDiffTime $ fromIntegral ?environment.timeout
        , setCookieSameSite = Just sameSiteStrict
        , setCookieHttpOnly = True
        , setCookieSecure = True
        }

setAuthFlowCookie ::
    (?environment :: Environment) =>
    Challenge ->
    Header
setAuthFlowCookie challenge =
    ( hSetCookie
    , renderSetCookieBS $ makeAuthFlowCookie challenge
    )

getAuthFlowCookie ::
    ( ?request :: Request
    , Monad m
    ) =>
    ExceptT String m Challenge
getAuthFlowCookie = do
    encoded <-
        maybeToExceptT
            "Missing the auth flow cookie"
            $ hoistMaybe
            $ ?request.cookie cookieName
    decoded <- except $ decode encoded
    pure $ makeChallenge decoded
