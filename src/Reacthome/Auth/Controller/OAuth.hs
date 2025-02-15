module Reacthome.Auth.Controller.OAuth where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.String
import Reacthome.Auth.Service.AuthFlow
import Reacthome.Auth.Service.AuthFlows
import Web.Rest
import Web.Rest.Status

oauth ::
    ( ?request :: Request
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
                _ <-
                    lift $
                        ?authFlows.start
                            AuthCodeGrant
                                { scope
                                , state
                                , redirect_uri
                                , client_id
                                }
                redirect mempty "/authentication"
            else throwE "Unknown type of the OAuth2 authorization flow"
  where
    query err name = maybeToExceptT err $ hoistMaybe $ ?request.query name
    missParam name = "Missing parameter `" <> name <> "` of the `AuthorizationCodeGrant` flow"
    flowParam name =
        query (missParam name) $ fromString name
