module Reacthome.Auth.Controller.OAuth where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.String
import Network.HTTP.Types.Header (hSetCookie)
import Reacthome.Auth.Controller.AuthFlowCookie
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Web.Cookie
import Web.Rest
import Web.Rest.Status

{-
    TODO: Should validate the `redirect_uri` parameter
-}

oauth ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authFlows :: AuthFlows
    ) =>
    IO Response
oauth = do
    either badRequest pure =<< runExceptT do
        responseType <- query "Not a valid `OAuth2` flow" "response_type"
        if responseType == "code"
            then do
                let scope = ?request.query "scope"
                state <- flowParam "state"
                redirect_uri <- flowParam "redirect_uri"
                client_id <- flowParam "client_id"
                challenge <-
                    lift $
                        ?authFlows.start
                            AuthCodeGrant
                                { scope
                                , state
                                , redirect_uri
                                , client_id
                                }
                let cookie = makeAuthFlowCookie challenge
                lift $ print cookie
                redirect [(hSetCookie, renderSetCookieBS cookie)] "/authentication"
            else throwE "Unknown type of the OAuth2 authorization flow"
  where
    query err name = maybeToExceptT err $ hoistMaybe $ ?request.query name
    missParam name = "Missing parameter `" <> name <> "` of the `AuthorizationCodeGrant` flow"
    flowParam name =
        query (missParam name) $ fromString name
