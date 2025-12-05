module Reacthome.Auth.Controller.OAuth where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.String
import Reacthome.Auth.Controller.AuthFlowCookie
import Reacthome.Auth.Environment
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Rest
import Rest.Status

{-
    TODO: Should validate the `redirect_uri` parameter
-}

oauth ::
    ( ?request :: Request
    , ?environment :: Environment
    , ?authFlows :: AuthFlows
    ) =>
    ExceptT String IO Response
oauth = do
    responseType <- query "Not a valid `OAuth2` flow" "response_type"
    if responseType == "code"
        then do
            let scope = ?request.query "scope"
            state <- flowParam "state"
            redirect_uri <- flowParam "redirect_uri"
            {-
                TODO: Check the `client_id` on the every stage?
            -}
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
            redirect [setAuthFlowCookie challenge] "/authentication"
        else throwE "Unknown type of the OAuth2 authorization flow"
  where
    query err name = maybeToExceptT err $ hoistMaybe $ ?request.query name
    missParam name = "Missing parameter `" <> name <> "` of the `AuthorizationCodeGrant` flow"
    flowParam name =
        query (missParam name) $ fromString name
